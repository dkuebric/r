rq = read.table("~/Downloads/request_times.txt", header=FALSE)$V1

run_simulation = function(router_mode = "naive", reqs_per_minute = 9000, simulation_length_in_minutes = 5, dyno_count = 100) {
  if(!(router_mode %in% c("naive", "intelligent"))) {
    return("router_mode must be one of 'naive' or 'intelligent'")
  }

  reqs_per_ms = reqs_per_minute / 60000
  simulation_length_in_ms = simulation_length_in_minutes * 60000

  # reqs_starting is a vector where reqs_starting[i] represents the number of requests that start at millisecond i
  reqs_starting = rpois(simulation_length_in_ms, reqs_per_ms)

  # total number of requests for duration of simulation
  total_requests = sum(reqs_starting)

  # req_durations_in_ms[i] represents the amount of time, in milliseconds, that request i will take to finish after a dyno starts working on it
  req_durations_in_ms = sample(rq, total_requests, TRUE)

  # For our simulation we used an empirical distribution of request times observed from our server, but we also
  # found that the Weibull distribution with shape parameter = 0.46 is a reasonable approximation.
  # You can change the code below to use whatever distribution of request times you'd like
  # wshape = 0.46
  # wlambda = 50 / (log(2) ^ (1 / wshape))
  # req_durations_in_ms = pmin(30000, pmax(10, ceiling(rweibull(total_requests, wshape, wlambda))))

  # The code below sets up the results matrix, which has one row for each request and will eventually have 4 columns of data:
  # 1) request start time
  # 2) request duration
  # 3) to which dyno was the request assigned?
  # 4) how much time, if any, did the request spend in queue between when the request arrived and when a dyno started working on it?
  # we can fill columns 1 and 2 based on results from above, and if we're in "naive" mode we can additionally fill column 3
  # the rest of the code will calculate the values for column 4

  uniq_start_times = which(reqs_starting > 0)
  start_times = unlist(sapply(uniq_start_times, function(x) rep(x, reqs_starting[x])))

  if(router_mode == "naive") {
    dyno_assignments = sample(1:dyno_count, total_requests, replace=TRUE)
  }
  else {
    dyno_assignments = rep(NA, total_requests)
  }

  results = matrix(c(start_times,
                    req_durations_in_ms,
                    dyno_assignments,
                    rep(0, total_requests)),
                  nrow = total_requests,
                  ncol = 4,
                  dimnames = list(1:total_requests, c("start_time", "request_duration", "dyno", "time_in_queue")))

  # dyno_next_available[i] represents the next millisecond at which dyno i will be free to being working on a new request
  # for example, if dyno 1 gets a request at time = 100 ms and the request lasts 55 ms, then dyno_next_available[1] will be set to 155
  dyno_next_available = rep(0, dyno_count)

  for(i in 1:nrow(results)) {
    row = results[i,]
    st = row["start_time"]
    duration = row["request_duration"]

    if(router_mode == "naive") {
      dyno = row["dyno"]
    }
    else {
      # if we're in 'intelligent' mode, assign task to the first dyno that is available (i.e. not working on some other request)
      dyno = which(dyno_next_available <= st)[1]
    }

    # if we've assigned a dyno and that dyno is available, then the request is not queued, and the dyno is tied up until the request is finished
    if(!is.na(dyno) & dyno_next_available[dyno] <= st) {
      dyno_next_available[dyno] = st + duration
    }
    # otherwise the request will be queued
    else {
      # 'intelligent' queueing will assign the request to the next dyno that comes available
      if(router_mode == "intelligent") {
        dyno = which.min(dyno_next_available)
      }

      queue_time = dyno_next_available[dyno] - st
      results[i, "time_in_queue"] = queue_time
      dyno_next_available[dyno] = st + queue_time + duration
    }
  }

  return(results)
}

frac_queued = function(result) {
  qtimes = result[, "time_in_queue"]

  data.frame(frac_queued = round(mean(qtimes > 0), 3),
             mean_queue_time_when_queued = round(mean(qtimes[qtimes > 0])),
             mean_queue_time_total_per_request = round(mean(qtimes)),
             median_queue_time_when_queued = round(as.numeric(median(qtimes[qtimes > 0]))),
             median_queue_time_total_per_request = round(as.numeric(median(qtimes))))
}

get_results = function(router_mode, dyno_count) {
  tmp = frac_queued(run_simulation(router_mode = router_mode, dyno_count = dyno_count))
  tmp$router_type = router_mode
  tmp$dyno_count = dyno_count
  return(tmp[, c(6:7, 1:5)])
}

results = c()
tests = list(c("intelligent", 50), c("intelligent", 60), c("intelligent", 75),
             c("naive", 75), c("naive", 100), c("naive", 150), c("naive", 200),
             c("naive", 500), c("naive", 1000), c("naive", 5000), c("naive", 10000))

for(t in tests) {
  results = rbind(results, get_results(t[1], as.numeric(t[2])))
}

results

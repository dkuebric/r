run_simulation = function(router_mode = "naive",
                          reqs_per_minute = 9000,
                          simulation_length_in_minutes = 5,
                          dyno_count = 100,
                          choice_of_two = FALSE,
                          power_of_two = FALSE,
                          unicorn_workers_per_dyno = 0,
                          track_dyno_queues = FALSE) {

  if(!(router_mode %in% c("naive", "intelligent"))) {
    return("router_mode must be one of 'naive' or 'intelligent'")
  }

  unicorn = as.numeric(unicorn_workers_per_dyno) > 0

  if(sum(c(choice_of_two, power_of_two, unicorn)) > 1) {
    return("you can only set one of choice_of_two, power_of_two, and unicorn!")
  }

  reqs_per_ms = reqs_per_minute / 60000
  simulation_length_in_ms = ceiling(simulation_length_in_minutes * 60000)

  # reqs_starting is a vector where reqs_starting[i] represents the number of requests that start at millisecond i
  reqs_starting = rpois(simulation_length_in_ms, reqs_per_ms)

  # total number of requests for duration of simulation
  total_requests = sum(reqs_starting)

  # req_durations_in_ms[i] represents the amount of time, in milliseconds, that request i will take to finish after a dyno starts working on it
  # req_durations_in_ms = sample(rq, total_requests, TRUE)
  req_durations_in_ms = ceiling(rweibull(n = total_requests, shape = 0.8, scale = 79.056))

  # For our simulation we used an empirical distribution of request times observed from our server, but we also
  # found that the Weibull distribution with shape parameter = 0.46 is a reasonable approximation.
  # You can change the code below to use whatever distribution of request times you'd like
  # wshape = 0.46
  # wlambda = 50 / (log(2) ^ (1 / wshape))
  # req_durations_in_ms = pmin(30000, pmax(10, ceiling(rweibull(nreqs, wshape, wlambda))))

  # the code below sets up the results matrix, which has one row for each request and will eventually have 4 columns of data:
  # 1) request start time
  # 2) request duration
  # 3) to which dyno was the request assigned?
  # 4) how much time, if any, did the request spend in queue between when the request arrived and when a dyno started working on it?
  # we can fill columns 1 and 2 based on results from above, and if we're in "naive" mode we can additionally fill column 3
  # the rest of the code will be calculate the values for column 4

  uniq_start_times = which(reqs_starting > 0)
  start_times = unlist(sapply(uniq_start_times, function(x) rep(x, reqs_starting[x])))

  if(router_mode == "naive") {
    dyno_assignments = sample(1:dyno_count, total_requests, replace=TRUE)
  } else {
    dyno_assignments = rep(NA, total_requests)
  }

  results = matrix(c(start_times,
                    req_durations_in_ms,
                    dyno_assignments,
                    rep(0, total_requests),
                    rep(0, total_requests)),
                  nrow = total_requests,
                  ncol = 5,
                  dimnames = list(1:total_requests, c("start_time", "request_duration", "dyno", "time_in_queue", "end_time")))

  # dyno_next_available[i] represents the next millisecond at which dyno i will be free to being working on a new request
  # for example, if dyno 1 gets a request at time = 100 ms and the request lasts 55 ms, then dyno_next_available[1] will be set to 155
  dyno_next_available = rep(0, dyno_count)

  # have to track dyno queues if you want to do power of two
  # also might want to track dyno queues if, for example, you want to plot or animate the simulation
  if(power_of_two) track_dyno_queues = TRUE

  if(track_dyno_queues) {
    # dynomat[i,j] represents the number of requests assigned to dyno i at time j
    dynomat = matrix(0, nrow = dyno_count, ncol = simulation_length_in_ms)
  }

  if(router_mode == "naive" & unicorn) {
    dyno_next_available = rep(dyno_next_available, unicorn_workers_per_dyno)
  }

  for(i in 1:nrow(results)) {
    row = results[i,]
    st = row["start_time"]
    duration = row["request_duration"]

    if(router_mode == "naive") {
      dyno = row["dyno"]

      # if using the choice of two approach, check if the random dyno is busy, and if it is then pick another random dyno
      if(choice_of_two & dyno_next_available[dyno] > st) {
        dyno = sample(1:dyno_count, 1)
        results[i, "dyno"] = dyno
      }

      # if using power of two and the first dyno is busy, poll a second dyno and pick the one that has a shorter queue depth
      if(power_of_two & dyno_next_available[dyno] > st) {
        other_dyno = sample(1:dyno_count, 1)

        dyno_queue_depth = dynomat[dyno, st]
        other_dyno_queue_depth = dynomat[other_dyno, st]

        if(other_dyno_queue_depth < dyno_queue_depth) {
          dyno = other_dyno
          results[i, "dyno"] = dyno
        }
      }

      # if using unicorn, pick a dyno at random, but then assign the request to a worker on that dyno based on which worker first comes available
      if(unicorn) {
        sub_dynos = seq((dyno - 1) * unicorn_workers_per_dyno + 1, length = unicorn_workers_per_dyno)
        dyno = as.numeric(sub_dynos[which(dyno_next_available[sub_dynos] <= st)[1]])
      }
    }
    else {
      # if we're in 'intelligent' mode, assign task to the first dyno that is available (i.e. not working on some other request)
      dyno = which(dyno_next_available <= st)[1]
    }

    # if we've assigned a dyno and that dyno is available, then the request is not queued, and the dyno is tied up until the request is finished
    if(!is.na(dyno) & dyno_next_available[dyno] <= st) {
      dyno_next_available[dyno] = st + duration
      results[i, "end_time"] = st + duration - 1

      if(track_dyno_queues) {
        t_ix = st:min(st + duration - 1, simulation_length_in_ms)
        dynomat[dyno, t_ix] = dynomat[dyno, t_ix] + 1
      }
    }
    # otherwise the request will be queued
    else {
      # 'intelligent' queueing will assign the request to the next dyno that comes available
      if(router_mode == "intelligent") {
        dyno = which.min(dyno_next_available)
      }

      if(router_mode == "naive" & unicorn) {
        dyno = as.numeric(sub_dynos[which.min(dyno_next_available[sub_dynos])])
      }

      queue_time = dyno_next_available[dyno] - st
      results[i, "time_in_queue"] = queue_time
      results[i, "end_time"] = st + queue_time + duration - 1
      dyno_next_available[dyno] = st + queue_time + duration

      if(track_dyno_queues) {
        t_ix = st:min(st + queue_time + duration - 1, simulation_length_in_ms)
        dynomat[dyno, t_ix] = dynomat[dyno, t_ix] + 1
      }
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

f_q_n = function(result) {
  qtimes = result[, "time_in_queue"]

  round(mean(qtimes > 0), 3)
}

get_results = function(router_mode, dyno_count, choice_of_two = FALSE, unicorn_workers_per_dyno = 0, power_of_two = FALSE) {

  tmp = frac_queued(run_simulation(router_mode = router_mode, dyno_count = dyno_count, choice_of_two = choice_of_two, power_of_two = power_of_two, unicorn_workers_per_dyno = unicorn_workers_per_dyno, reqs_per_minute = 9000, simulation_length_in_minutes = 3))


  opts = if(choice_of_two) {
    "choice of two"
  } else if(power_of_two) {
    "power_of_two"
  } else if(unicorn_workers_per_dyno > 0) {
    paste("unicorn", unicorn_workers_per_dyno, "workers per dyno", sep=" ")
  } else {
    ""
  }

  tmp$type = paste(router_mode, opts, dyno_count, sep=" ")
  tmp$router_type = router_mode
  tmp$dyno_count = dyno_count
  return(tmp[, c(6:8, 1:5)])
}

range_builder = function(min, max, by) {
    r = list(min=min, max=max, by=by)
    r$s = seq(r$min, r$max, r$by)
    r$len = length(r$s)
    r
}

# test computational-equivalent-power of # of dynos from 10 to 100
dynos = range_builder(10, 100, 5)
# ... using worker counts/node
workers = range_builder(2, 8, 2)

arr = array(0, c(2, dynos$len, workers$len))
labels = c()
colors = c("black", "green", "red", "blue")

d = 1
for (num_workers in workers$s) {
    labels = append(labels, paste(num_workers, "workers", sep=" "))
    n = 1
    for (num_dynos in dynos$s) {
        # approximately "fair" to total computational power.. let's see how good
        fair_d_count = ceiling(num_dynos / num_workers)
        res = get_results(router_mode = 'naive', dyno_count = fair_d_count, unicorn_workers_per_dyno = num_workers)
        arr[,n,d] = c(res$dyno_count, res$frac_queued)
        n = n + 1
    }
    d = d + 1
}

plot(x=arr[1,,1],
        y=arr[2,,1],
        col=colors[1],
        type='l',
        main='% of Requests Queued as Function of # of nodes\nrequest lengths as per\nrweibull(shape = 0.8, scale = 79.056)',
        xlab='# of nodes (eg. dynos)',
        ylab='fraction of requests queued')
points(x=arr[1,,2], y=arr[2,,2], col=colors[2], type='l')
points(x=arr[1,,3], y=arr[2,,3], col=colors[3], type='l')
points(x=arr[1,,4], y=arr[2,,4], col=colors[4], type='l')
legend(x=40,y=1, labels, cex=0.8, col=colors, lty=1)

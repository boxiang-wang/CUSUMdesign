CUSUMchart = function(summand, kup, kdn, hup, hdn, nameof, ylab) {
  eps = 1.e-10
  stoup = NULL
  stodn = NULL
  cuup  =  hup * 0.5
  cudn  = -hdn * 0.5
  lenof = length(summand)
  for (n in 1:lenof) {
    cuup  = max(c(0, cuup + summand[n] - kup)) 
    stoup = c(stoup, cuup)
    cudn  = min(c(0, cudn + summand[n] - kdn))
    stodn = c(stodn, cudn)
    cat(sprintf("%6.0f %7.2f %7.2f %7.2f \n", 
      n, summand[n], cuup, cudn))
  }
  ylim = c(min(c(-hdn, stodn)), max(c(hup, stoup)))
  plot((1:lenof), stoup, main=nameof, 
    ylim=ylim, xlab="Group", ylab=ylab, type="o")
  
  lines((1:lenof), stodn, type="o", lty=2)
  abline(h=0)
  abline(h=hup, lty=3)
  abline(h=-hdn, lty=3)  
  
  upsignal = min((1:(lenof+1))[c(stoup,1e10)  >  hup-eps])
  dnsignal = min((1:(lenof+1))[c(stodn,-1e10) <  -hdn+eps])
  signal   = min(c(upsignal, dnsignal))
  lastIC   = 0
  newmean  = 0
  if(signal <= lenof) {        # have a signal.  Diagnose
    if (upsignal < dnsignal) {                    # is upward
      lastIC = max((1:signal)[abs(stoup) < eps])
      newmean = kup + stoup[signal]/(signal-lastIC)
    } else {
      lastIC = max((1:signal)[abs(stodn) < eps])
      newmean = kdn + stodn[signal]/(signal-lastIC)
    }   
    cat(sprintf("Signal %4.0f last estimated IC %4.0f new mean estimate %6.2f \n",
     signal, lastIC, newmean))
  }
  return(list(lastIC=lastIC, newmean=newmean))  
}

# Copyright (c) 2015 Santiago Barreda
# All rights reserved.


plot.sound = function (x, ...){
  if (!exists("xlab")) xlab = 'Time (s)'
  if (!exists("ylab")) ylab = 'Amplitude'

  if (!inherits(x$sound,'ts')) plot ((1:length(x$sound))/x$fs, x$sound, xlab=xlab, 
                                   ylab=ylab, xaxs = 'i', type = 'l', ...)
  if (inherits(x$sound,'ts')) plot (x$sound, xlab=xlab, ylab=ylab, xaxs = 'i', ...)
}

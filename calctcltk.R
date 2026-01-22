library(tcltk)
calcinp <- ''

mkput <- function(sym){
  function(){
    calcinp <<- paste(calcinp, sym, sep='')
    tkconfigure(display, text = calcinp)
  }
}

clearit <- function(){
  tkconfigure(display, text = '')
  calcinp <<- ''
}

docalc <- function(){
  result = try(eval(parse(text = calcinp)))
  if(class(result) == 'try-error')
    calcinp <<- 'Error' 
  else calcinp <<- result
  tkconfigure(display, text = calcinp)
  calcinp <<- ''
}


base <- tktoplevel()
tkwm.title(base,'Calculator')

tkpack(tklabel(base))
display <- tklabel(base, justify='right', background = "white", relief = "sunken", padx = 50)
tkpack(display,tklabel(base),side='top')
row1 <- tkframe(base)
tkpack(tkbutton(row1,text='7',command=mkput('7'),width=3,background = "lightgreen", foreground = "red"),side='left')
tkpack(tkbutton(row1,text='8',command=mkput('8'),width=3,background = "lightgreen", foreground = "red"),side='left')
tkpack(tkbutton(row1,text='9',command=mkput('9'),width=3,background = "lightgreen", foreground = "red"),side='left')
tkpack(tkbutton(row1,text='+',command=mkput('+'),width=3,background = "orange", foreground = "blue"),side='left')
tkpack(row1,side='top')

row2 <- tkframe(base)
tkpack(tkbutton(row2,text='4',command=mkput('4'),width=3,background = "lightgreen", foreground = "red"),side='left')
tkpack(tkbutton(row2,text='5',command=mkput('5'),width=3,background = "lightgreen", foreground = "red"),side='left')
tkpack(tkbutton(row2,text='6',command=mkput('6'),width=3,background = "lightgreen", foreground = "red"),side='left')
tkpack(tkbutton(row2,text='-',command=mkput('-'),width=3,background = "orange", foreground = "blue"),side='left')
tkpack(row2,side='top')

row3 <- tkframe(base)
tkpack(tkbutton(row3,text='1',command=mkput('1'),width=3,background = "lightgreen", foreground = "red"),side='left')
tkpack(tkbutton(row3,text='2',command=mkput('2'),width=3,background = "lightgreen", foreground = "red"),side='left')
tkpack(tkbutton(row3,text='3',command=mkput('3'),width=3,background = "lightgreen", foreground = "red"),side='left')
tkpack(tkbutton(row3,text='*',command=mkput('*'),width=3,background = "orange", foreground = "blue"),side='left')
tkpack(row3,side='top')

row4 <- tkframe(base)
tkpack(tkbutton(row4,text='(',command=mkput('('),width=3,background = "blue", foreground = "white"),side='left')
tkpack(tkbutton(row4,text='0',command=mkput('0'),width=3,background = "lightgreen", foreground = "red"),side='left')
tkpack(tkbutton(row4,text=')',command=mkput(')'),width=3,background = "blue", foreground = "white"),side='left')
tkpack(tkbutton(row4,text='/',command=mkput('/'),width=3,background = "orange", foreground = "blue"),side='left')
tkpack(row4,side='top')

row5 <- tkframe(base)
tkpack(tkbutton(row5,text='.',command=mkput('.'),width=3,background = "blue", foreground = "white"),side='left')
tkpack(tkbutton(row5,text='^',command=mkput('^'),width=3,background = "blue", foreground = "white"),side='left')
tkpack(tkbutton(row5,text='C',command=clearit,width=3,background = "blue", foreground = "white"),side='left')
tkpack(tkbutton(row5,text='=',command=docalc,width=3,background = "orange", foreground = "blue"),side='left')
tkpack(row5,side='top')

tkpack(tklabel(base))
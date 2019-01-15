#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

# creates the U|IND{d} elements

ExD=lapply(1:NROW(Example),function(x)(subset(Example,Example[,5]==Example[x,5])))
ExD=ExD[duplicated(ExD)==FALSE]

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

# creates the U|IND{a, c} elements

ExAC=lapply(1:NROW(Example),function(x)(subset(Example,Example[,2]==Example[x,2]&Example[,4]==Example[x,4])))
ExAC=ExAC[duplicated(ExAC)==FALSE]

#-----

# creates P(X_i) for U|IND{a, c}

ExP_X.AC=sapply(1:length(ExAC),function(x)(NROW(ExAC[[x]])/NROW(Example)))

#-----

# creates P(Y_1|X_i) for U|IND{a, c}

ExP_Y1_X.AC=sapply(1:length(ExAC),function(x)(sum(ifelse(duplicated(rbind(ExAC[[x]],ExD[[2]]))==TRUE,1,0))/NROW(ExAC[[x]])))

#-----

# creates P(Y_2|X_i) for U|IND{a, c}

ExP_Y2_X.AC=sapply(1:length(ExAC),function(x)(sum(ifelse(duplicated(rbind(ExAC[[x]],ExD[[1]]))==TRUE,1,0))/NROW(ExAC[[x]])))

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

# creates the U|IND{b, c} elements

ExBC=lapply(1:NROW(Example),function(x)(subset(Example,Example[,3]==Example[x,3]&Example[,4]==Example[x,4])))
ExBC=ExBC[duplicated(ExBC)==FALSE]

#-----

# creates P(X_i) for U|IND{b, c}

ExP_X.BC=sapply(1:length(ExBC),function(x)(NROW(ExBC[[x]])/NROW(Example)))

#-----

# creates P(Y_1|X_i) for U|IND{b, c}

ExP_Y1_X.BC=sapply(1:length(ExBC),function(x)(sum(ifelse(duplicated(rbind(ExBC[[x]],ExD[[2]]))==TRUE,1,0))/NROW(ExBC[[x]])))

#-----

# creates P(Y_2|X_i) for U|IND{b, c}

ExP_Y2_X.BC=sapply(1:length(ExBC),function(x)(sum(ifelse(duplicated(rbind(ExBC[[x]],ExD[[1]]))==TRUE,1,0))/NROW(ExBC[[x]])))

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

# creates the U|IND{a, b} elements

ExAB=lapply(1:NROW(Example),function(x)(subset(Example,Example[,2]==Example[x,2]&Example[,3]==Example[x,3])))
ExAB=ExAB[duplicated(ExAB)==FALSE]

#-----

# creates P(X_i) for U|IND{a, b}

ExP_X.AB=sapply(1:length(ExAB),function(x)(NROW(ExAB[[x]])/NROW(Example)))

#-----

# creates P(Y_1|X_i) for U|IND{a, b}

ExP_Y1_X.AB=sapply(1:length(ExAB),function(x)(sum(ifelse(duplicated(rbind(ExAB[[x]],ExD[[2]]))==TRUE,1,0))/NROW(ExAB[[x]])))

#-----

# creates P(Y_2|X_i) for U|IND{a, b}

ExP_Y2_X.AB=sapply(1:length(ExAB),function(x)(sum(ifelse(duplicated(rbind(ExAB[[x]],ExD[[1]]))==TRUE,1,0))/NROW(ExAB[[x]])))

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

# creates the U|IND{e} elements

ExE=lapply(1:NROW(Example),function(x)(subset(Example,Example[,6]==Example[x,6])))
ExE=ExE[duplicated(ExE)==FALSE]

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

# creates the U|IND{a, b, c} elements

ExABC=lapply(1:NROW(Example),function(x)(subset(Example,Example[,2]==Example[x,2]&Example[,3]==Example[x,3]&Example[,4]==Example[x,4])))
ExABC=ExABC[duplicated(ExABC)==FALSE]

#-----

# creates P(X_i) for U|IND{a, b, c}

ExP_X.ABC=sapply(1:length(ExABC),function(x)(NROW(ExABC[[x]])/NROW(Example)))

#-----

# creates P(Y_1|X_i) for U|IND{a, b, c}

ExP_Y1_X.ABC=sapply(1:length(ExABC),function(x)(sum(ifelse(duplicated(rbind(ExABC[[x]],ExE[[2]]))==TRUE,1,0))/NROW(ExABC[[x]])))

#-----

# creates P(Y_2|X_i) for U|IND{a, b, c}

ExP_Y2_X.ABC=sapply(1:length(ExABC),function(x)(sum(ifelse(duplicated(rbind(ExABC[[x]],ExE[[1]]))==TRUE,1,0))/NROW(ExABC[[x]])))

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

# creates the U|IND{a, b, d} elements

ExABD=lapply(1:NROW(Example),function(x)(subset(Example,Example[,2]==Example[x,2]&Example[,3]==Example[x,3]&Example[,5]==Example[x,5])))
ExABD=ExABD[duplicated(ExABD)==FALSE]

#-----

# creates P(X_i) for U|IND{a, b, d}

ExP_X.ABD=sapply(1:length(ExABD),function(x)(NROW(ExABD[[x]])/NROW(Example)))

#-----

# creates P(Y_1|X_i) for U|IND{a, b, d}

ExP_Y1_X.ABD=sapply(1:length(ExABD),function(x)(sum(ifelse(duplicated(rbind(ExABD[[x]],ExE[[2]]))==TRUE,1,0))/NROW(ExABD[[x]])))

#-----

# creates P(Y_2|X_i) for U|IND{a, b, d}

ExP_Y2_X.ABD=sapply(1:length(ExABD),function(x)(sum(ifelse(duplicated(rbind(ExABD[[x]],ExE[[1]]))==TRUE,1,0))/NROW(ExABD[[x]])))

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

# creates the U|IND{a, c, d} elements

ExACD=lapply(1:NROW(Example),function(x)(subset(Example,Example[,2]==Example[x,2]&Example[,4]==Example[x,4]&Example[,5]==Example[x,5])))
ExACD=ExACD[duplicated(ExACD)==FALSE]

#-----

# creates P(X_i) for U|IND{a, c, d}

ExP_X.ACD=sapply(1:length(ExACD),function(x)(NROW(ExACD[[x]])/NROW(Example)))

#-----

# creates P(Y_1|X_i) for U|IND{a, c, d}

ExP_Y1_X.ACD=sapply(1:length(ExACD),function(x)(sum(ifelse(duplicated(rbind(ExACD[[x]],ExE[[2]]))==TRUE,1,0))/NROW(ExACD[[x]])))

#-----

# creates P(Y_2|X_i) for U|IND{a, c, d}

ExP_Y2_X.ACD=sapply(1:length(ExACD),function(x)(sum(ifelse(duplicated(rbind(ExACD[[x]],ExE[[1]]))==TRUE,1,0))/NROW(ExACD[[x]])))

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------

# creates the U|IND{b, c, d} elements

ExBCD=lapply(1:NROW(Example),function(x)(subset(Example,Example[,3]==Example[x,3]&Example[,4]==Example[x,4]&Example[,5]==Example[x,5])))
ExBCD=ExBCD[duplicated(ExBCD)==FALSE]

#-----

# creates P(X_i) for U|IND{b, c, d}

ExP_X.BCD=sapply(1:length(ExBCD),function(x)(NROW(ExBCD[[x]])/NROW(Example)))

#-----

# creates P(Y_1|X_i) for U|IND{b, c, d}

ExP_Y1_X.BCD=sapply(1:length(ExBCD),function(x)(sum(ifelse(duplicated(rbind(ExBCD[[x]],ExE[[2]]))==TRUE,1,0))/NROW(ExBCD[[x]])))

#-----

# creates P(Y_2|X_i) for U|IND{b, c, d}

ExP_Y2_X.BCD=sapply(1:length(ExBCD),function(x)(sum(ifelse(duplicated(rbind(ExBCD[[x]],ExE[[1]]))==TRUE,1,0))/NROW(ExBCD[[x]])))

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------



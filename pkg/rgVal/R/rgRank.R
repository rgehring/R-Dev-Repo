#  /***********************************/ #
#  /*            _           _        */ #
#  /*          /\ \        /\ \       */ #
#  /*         /  \ \      /  \ \      */ #
#  /*        / /\ \ \    / /\ \_\     */ #
#  /*       / / /\ \_\  / / /\/_/     */ #
#  /*      / / /_/ / / / / / ______   */ #
#  /*     / / /__\/ / / / / /\_____\  */ #
#  /*    / / /_____/ / / /  \/____ /  */ #
#  /*   / / /\ \ \  / / /_____/ / /   */ #
#  /*  / / /  \ \ \/ / /______\/ /    */ #
#  /*  \/_/    \_\/\/___________/     */ #
#  /*                                 */ #
#  /*     P R O D U C T I O N S       */ #
#  /*                                 */ #
#  /***********************************/ #
#  /*     R Y A N   G E H R I N G     */ #
#  /***********************************/ #


is.wholenumber =
  function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

rgRank = function(data, nTiles) { 
  if (nTiles > length(data)) stop("More tiles than data you dummy.") 
  if (!is.wholenumber(nTiles) | nTiles <1) stop("nTiles must be positive int")
  
  rankFunc=ecdf(data)
  out = 1+trunc(nTiles*rankFunc(data))
  out[which(is.wholenumber(nTiles*rankFunc(data)  ))]= nTiles*rankFunc(data[which(is.wholenumber(nTiles*rankFunc(data)  ))])
return(out)
}


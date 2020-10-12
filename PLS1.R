
PLS <- function(X,y,l){
    X0 = X
    w0 = (t(X)*y)/norm(t(X)*y)
    W=w0
    for (k in 0:(l-1)){
      t0 = X0 * w0
      ts = t(t0)*t0 #ts adalah t scalar
      t0 = t0 / ts
      p0 = t(X0) * t0
      q0 = t(y)*t0
      if (q0 == 0){
        break()}
      if (k<(l-1)){
        X1 = X0 - (ts*t0*t(p0))
        w1 = t(X1)* y
        W=cbind(W,w1)
        X0 = X1
        w0 = w1}
      
      P=cbind(p0)
    }
    W #matriks isinya w0 sampai w l-1
    P # juga sama
    Q #vektor juga sama
    B = W* solve((t(P)*W)) * Q
    B0 = q0 - (t(p0)*B)
    return(B,B0)
  }


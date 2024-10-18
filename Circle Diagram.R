library(ggplot2)

# Set theta value
theta <- pi / 3

# Circle data
circle_data <- data.frame(
  x = cos(seq(0, 2 * pi, length.out = 100)),
  y = sin(seq(0, 2 * pi, length.out = 100))
)

# Coordinates
x_theta <- cos(theta)
y_theta <- sin(theta)

O <- c(0, 0)
A <- c(x_theta, y_theta)
A_extra <- c(1.5 * x_theta, 1.5 * y_theta)
B <- c(x_theta, -y_theta)
C <- c(x_theta, 0)
D <- c(1, 0)
E <- c(tan(theta), 0)
F <- c(0, 1.2)
G <- c(0, y_theta)
H <- c(0, 1)
K <- c(-1, 0)
L <- c(0, -1)
M <- c(0, -0.5)
N <- c(-0.5, 0)

plot <- ggplot() +
  
  # Circle
  geom_path(data = circle_data, aes(x = x, y = y), colour = "black", size = 2.5) +
  
  # Theta line (O to A)
  geom_segment(aes(x = O[1], y = O[2], xend = A[1], yend = A[2]), colour = "black", size = 1.2) +
  
  # Dotted theta extra line (A to A_extra)
  geom_segment(aes(x = A[1], y = A[2], xend = A_extra[1], yend = A_extra[2]), colour = "black", linetype = "dotted", size = 1.2) +
  
  # Mirror theta line (O to B)
  geom_segment(aes(x = O[1], y = O[2], xend = B[1], yend = B[2]), colour = "black", linetype = "longdash", size = 1.2) +
  
  # Red vertical lines (A to C and O to G)
  geom_segment(aes(x = A[1], y = A[2], xend = C[1], yend = C[2]), colour = "red", size = 1.2) +
  geom_segment(aes(x = O[1], y = O[2], xend = G[1], yend = G[2]), colour = "red", size = 1.2) +
  
  # Blue cos line (O to C)
  geom_segment(aes(x = O[1], y = O[2], xend = C[1], yend = C[2]), colour = "blue", size = 1.2) +
  
  # Green versin line (C to D)
  geom_segment(aes(x = C[1], y = C[2], xend = D[1], yend = D[2]), colour = "green", size = 1.2) +
  
  # Cyan coversin line (G to H)
  geom_segment(aes(x = G[1], y = G[2], xend = H[1], yend = H[2]), colour = "cyan", size = 1.2) +
  
  # Dark green exsec line (H to F)
  geom_segment(aes(x = H[1], y = H[2], xend = F[1], yend = F[2]), colour = "darkgreen", size = 1.2) +
  
  # OL line (O to L)
  geom_segment(aes(x = O[1], y = O[2], xend = M[1], yend = M[2]), colour = "turquoise", linetype = "dotted", size = 1.2) +
  geom_segment(aes(x = M[1], y = M[2], xend = L[1], yend = L[2]), colour = "lightgray", linetype = "longdash", size = 1.2) +
  
  # OK line (O to K)
  geom_segment(aes(x = O[1], y = O[2], xend = N[1], yend = N[2]), colour = "lightpink", linetype = "dotted", size = 1.2) +
  geom_segment(aes(x = N[1], y = N[2], xend = K[1], yend = K[2]), colour = "lightgray", linetype = "longdash", size = 1.2) + 
  
  # CB line (B to C)
  geom_segment(aes(x = B[1], y = B[2], xend = C[1], yend = C[2]), colour = "lightgray", linetype = "longdash", size = 1.2) +
  
  # Orange cotangent line (F to A)
  geom_segment(aes(x = F[1], y = F[2], xend = A[1], yend = A[2]), colour = "orange", size = 1.2) +
  
  # Tan tangent line (E to A)
  geom_segment(aes(x = E[1], y = E[2], xend = A[1], yend = A[2]), colour = "tan", size = 1.2) +
  
  # Light pink exsec line (F to K) with arrows
  geom_segment(aes(x = -0.5, y = F[2], xend = -0.5, yend = 0), colour = "lightpink", size = 1.2,
               arrow = arrow(type = "closed", ends = "both", length = unit(0.2, "inches"))) +
  
  coord_fixed() +
  theme_void()

print(plot)

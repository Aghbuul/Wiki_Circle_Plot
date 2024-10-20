library(ggplot2)

# Set theta value
theta <- pi / 3

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
  geom_path(data = data.frame(
    x = cos(seq(0, 2 * pi, length.out = 100)),
    y = sin(seq(0, 2 * pi, length.out = 100))
  ), 
  aes(x = x, y = y), colour = "black", linewidth = 1.2) +
  
  # Theta line (O to A)
  geom_segment(aes(x = O[1], y = O[2], xend = A[1], yend = A[2]), colour = "black", linewidth = 1.2) +
  
  # Dotted theta extra line (A to A_extra)
  geom_segment(aes(x = A[1], y = A[2], xend = A_extra[1], yend = A_extra[2]), colour = "black", linetype = "dotted", linewidth = 1.2) +
  
  # Mirror theta line (O to B)
  geom_segment(aes(x = O[1], y = O[2], xend = B[1], yend = B[2]), colour = "black", linetype = "longdash", linewidth = 1.2) +
  
  # Red vertical lines (A to C and O to G)
  geom_segment(aes(x = A[1], y = A[2], xend = C[1], yend = C[2]), colour = "red", linewidth = 1.2) +
  geom_segment(aes(x = O[1], y = O[2], xend = G[1], yend = G[2]), colour = "red", linewidth = 1.2) +
  geom_text(aes(x = A[1]+0.11, y = (O[2] + A[2]) / 2, label = "sin"), colour = "red", size = 7) +
  
  # Blue cos line
  geom_segment(aes(x = O[1], y = O[2], xend = C[1], yend = C[2]), colour = "blue", linewidth = 1.2) +
  geom_segment(aes(x = A[1], y = A[2], xend = G[1], yend = G[2]), colour = "blue", linewidth = 1.2) +
  geom_text(aes(x = (O[1] + C[1]) / 2, y = O[2] - 0.05, label = "cos"), colour = "blue", size = 7) +
  
  # Spring green versin line (C to D)
  geom_segment(aes(x = C[1], y = C[2], xend = D[1], yend = D[2]), colour = "springgreen3", linewidth = 1.2) +
  geom_text(aes(x = (C[1] + D[1]) / 2, y = C[2] - 0.05, label = "versin"), colour = "springgreen3", size = 7) +
  
  # Cyan coversin line (G to H)
  geom_segment(aes(x = G[1], y = G[2], xend = H[1], yend = H[2]), colour = "cyan", linewidth = 1.2) +
  geom_text(aes(x = -0.2, y = (G[2] + H[2]) / 2, label = "coversin"), colour = "cyan", size = 4) +
  
  # Dark green excsc line (H to F)
  geom_segment(aes(x = H[1], y = H[2], xend = F[1], yend = F[2]), colour = "darkgreen", linewidth = 1.2) +
  geom_text(aes(x = -0.2, y = (H[2] + F[2]) / 2, label = "excsc"), colour = "darkgreen", size = 6) +
  
  # OL line
  geom_segment(aes(x = O[1], y = O[2], xend = M[1], yend = M[2]), colour = "lightseagreen", linetype = "dotted", linewidth = 1.2) +
  geom_segment(aes(x = M[1], y = M[2], xend = L[1], yend = L[2]), colour = "gray", linetype = "longdash", linewidth = 1.2) +
  
  # OK line
  geom_segment(aes(x = O[1], y = O[2], xend = N[1], yend = N[2]), colour = "lightpink", linetype = "dotted", linewidth = 1.2) +
  geom_segment(aes(x = N[1], y = N[2], xend = K[1], yend = K[2]), colour = "gray", linetype = "longdash", linewidth = 1.2) + 
  
  # CB line
  geom_segment(aes(x = B[1], y = B[2], xend = C[1], yend = C[2]), colour = "gray", linetype = "longdash", linewidth = 1.2) +
  
  # Orange cotangent line (F to A)
  geom_segment(aes(x = F[1], y = F[2], xend = A[1], yend = A[2]), colour = "orange", linewidth = 1.2) +
  geom_segment(aes(x = F[1], y = F[2], xend = F[1]-0.55, yend = F[2]), colour = "orange", linetype = "dotted", linewidth = 1.2) +
  geom_text(aes(x = (F[1] + A[1]) / 2, y = F[2] - 0.05, label = "cot"), colour = "orange", size = 7, angle = -33) +
  
  # Tan tangent line (E to A)
  geom_segment(aes(x = E[1], y = E[2], xend = A[1], yend = A[2]), colour = "tan", linewidth = 1.2) +
  geom_text(aes(x = (E[1] + A[1]) / 2, y = (E[2] + A[2]) / 2 + 0.15, label = "tan"), colour = "tan", size = 7, angle = -33) +
  
  # Light pink csc line with arrows
  geom_segment(aes(x = -0.5, y = F[2], xend = -0.5, yend = 0), colour = "lightpink", linewidth = 1.2,
               arrow = arrow(type = "closed", ends = "both", length = unit(0.2, "inches"))) +
  geom_text(aes(x = -0.35, y = F[2] / 2, label = "csc"), colour = "lightpink", size = 7) +
  
  # Purple covercos line
  geom_segment(aes(x = -0.65, y = A[2], xend = -0.65, yend = -1), colour = "purple", linewidth = 1.2,
               arrow = arrow(type = "closed", ends = "both", length = unit(0.2, "inches"))) +
  geom_text(aes(x = -0.75, y = 0.15, label = "covercos"), colour = "purple", size = 7, angle = 90) +
  
  # Steel blue vercos line
  geom_segment(aes(x = K[1], y = K[2] - 0.25, xend = C[1], yend = C[2] - 0.25), colour = "steelblue", linewidth = 1.2,
               arrow = arrow(type = "closed", ends = "both", length = unit(0.2, "inches"))) +
  geom_text(aes(x = (K[1] + C[1]) / 2, y = K[2] - 0.3, label = "vercos"), colour = "steelblue", size = 7) +
  
  # Light sea green sec line
  geom_segment(aes(x = M[1], y = M[2] + 0.05, xend = E[1], yend = M[2] + 0.05), colour = "lightseagreen", linewidth = 1.2,
               arrow = arrow(type = "closed", ends = "both", length = unit(0.2, "inches"))) +
  geom_text(aes(x = (M[1] + E[1]) / 2, y = M[2] - 0.07, label = "sec"), colour = "lightseagreen", size = 7) +

  # Turquoise secant label line on the right
  geom_segment(aes(x = E[1], y = E[2], xend = E[1], yend = M[2]-0.02), colour = "lightseagreen", linetype = "dotted", linewidth = 1.2) +
  
  # Light gray crd line (A to D)
  geom_segment(aes(x = A[1], y = A[2], xend = D[1], yend = D[2]), colour = "lightgray", linewidth = 1.2) +
  geom_text(aes(x = (A[1] + D[1]) / 2 + 0.05, y = (A[2] + D[2]) / 2 + 0.05, label = "crd"), colour = "lightgray", size = 7, angle = -57) +
  
  # Pink exsec line
  geom_segment(aes(x = D[1], y = D[2], xend = E[1], yend = E[2]), colour = "pink", linewidth = 1.2) +
  geom_text(aes(x = (D[1] + E[1]) / 2 + 0.07, y = D[2]-0.05 , label = "exsec"), colour = "pink", size = 7) +
  
  # Light gray dotted extra line 
  geom_segment(aes(x = K[1], y = K[2], xend = K[1], yend = K[2] - 0.5), colour = "lightgray", linetype = "dotted", linewidth = 1.2) +
  geom_segment(aes(x = L[1], y = L[2], xend = L[1]- 0.8, yend = L[2]), colour = "lightgray", linetype = "dotted", linewidth = 1.2) +
  geom_segment(aes(x = G[1], y = G[2], xend = G[1]- 0.8, yend = G[2]), colour = "lightgray", linetype = "dotted", linewidth = 1.2) +

  # Right angle at A (there must be an easier way to do this...)
  geom_segment(aes(x = (F[1] - A[1]) / 9 + (A_extra[1] - A[1]) / 6 + A[1], y = (F[2] - A[2]) / 9 + (A_extra[2] - A[2]) / 6 + A[2], xend = (F[1] - A[1]) / 9 + A[1], yend = (F[2] - A[2]) / 9 + A[2]), linewidth = 1.2) +
  geom_segment(aes(x = (F[1] - A[1]) / 9 + (A_extra[1] - A[1]) / 6 + A[1], y = (F[2] - A[2]) / 9 + (A_extra[2] - A[2]) / 6 + A[2], xend = (A_extra[1] - A[1]) / 6 + A[1], yend = (A_extra[2] - A[2]) / 5 + A[2]), linewidth = 1.2) +

  # Right angle at C
  geom_segment(aes(x = C[1] - 0.1, y = C[2], xend = C[1] - 0.1, yend = C[2] + 0.1), colour = "black", linewidth = 1.2) +
  geom_segment(aes(x = C[1] - 0.1, y = C[2] + 0.1, xend = C[1], yend = C[2] + 0.1), colour = "black", linewidth = 1.2) +
  
  # Right angle at O
  geom_segment(aes(x = O[1] - 0.1, y = O[2], xend = O[1] - 0.1, yend = O[2] + 0.1), colour = "black", linewidth = 1.2) +
  geom_segment(aes(x = O[1] - 0.1, y = O[2] + 0.1, xend = O[1], yend = O[2] + 0.1), colour = "black", linewidth = 1.2) +
  
  # Right angle at G
  geom_segment(aes(x = G[1] + 0.1, y = G[2], xend = G[1] + 0.1, yend = G[2] - 0.1), colour = "black", linewidth = 1.2) +
  geom_segment(aes(x = G[1] + 0.1, y = G[2] - 0.1, xend = G[1], yend = G[2] - 0.1), colour = "black", linewidth = 1.2) +
  
  # Theta angle
  geom_path(data = data.frame(
    x = cos(seq(0, theta, length.out = 100)) * 0.1, y = sin(seq(0, theta, length.out = 100)) * 0.1), 
    aes(x = x, y = y), colour = "black", linewidth = 1) +
  geom_text(aes(x = 0.15, y = 0.1, label = "θ"), colour = "black", size = 7) +

  # Thick arc 
  geom_path(data = data.frame(
    x = cos(seq(0, theta, length.out = 100)), y = sin(seq(0, theta, length.out = 100))), 
    aes(x = x, y = y), colour = "black", linewidth = 2) +
  geom_text(aes(x = 1.12, y = 0.25, label = "arc"), colour = "black", size = 7) +

  # Add labels to each coordinate
  geom_text(aes(x = O[1] - 0.07, y = O[2] - 0.07, label = "O"), colour = "black", size = 7) +
  geom_text(aes(x = A[1] + 0.09, y = A[2] + 0.07, label = "A"), colour = "black", size = 7) +
  geom_text(aes(x = B[1] + 0.07, y = B[2] - 0.07, label = "B"), colour = "black", size = 7) +
  geom_text(aes(x = C[1] + 0.07, y = C[2] + 0.09, label = "C"), colour = "black", size = 7) +
  geom_text(aes(x = D[1] + 0.07, y = D[2] - 0.07, label = "D"), colour = "black", size = 7) +
  geom_text(aes(x = E[1] + 0.07, y = E[2] - 0.07, label = "E"), colour = "black", size = 7) +
  geom_text(aes(x = F[1] + 0.00, y = F[2] + 0.08, label = "F"), colour = "black", size = 7) +
  geom_text(aes(x = G[1] - 0.07, y = G[2] - 0.07, label = "G"), colour = "black", size = 7) +
  geom_text(aes(x = H[1] + 0.07, y = H[2] + 0.02, label = "H"), colour = "black", size = 7) +
  geom_text(aes(x = K[1] - 0.10, y = K[2] + 0.00, label = "K"), colour = "black", size = 7) +
  geom_text(aes(x = L[1] + 0.00, y = L[2] - 0.07, label = "L"), colour = "black", size = 7) +
  # I put M and N there to make it easier to set where the lines should go
  #geom_text(aes(x = M[1] + 0.07, y = M[2] + 0.07, label = "M"), colour = "black", size = 7) +
  #geom_text(aes(x = N[1] + 0.07, y = N[2] + 0.07, label = "N"), colour = "black", size = 7) +
  
  # Top right legend
  geom_text(aes(x = 1.2, y = 1.3, label = "sec=OE"), colour = "lightseagreen", size = 5, hjust = 0) +
  geom_text(aes(x = 1.2, y = 1.2, label = "csc=OF"), colour = "lightpink", size = 5, hjust = 0) +
  geom_text(aes(x = 1.2, y = 1.1, label = "vercos=CK"), colour = "steelblue", size = 5, hjust = 0) +
  geom_text(aes(x = 1.2, y = 1.0, label = "coversin=GH"), colour = "cyan", size = 5, hjust = 0) +
  geom_text(aes(x = 1.2, y = 0.9, label = "covercos=GL"), colour = "purple", size = 5, hjust = 0) +
  
  coord_fixed() +
  theme_void()

print(plot)

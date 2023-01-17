

# Mcnemar 1

p_value = c(0.0005, 0.001953, 0.625, 0.125, 0.031, 0.688, 0.003, 0.006, 1)

p.adjust(p_value, method = 'BH')

#p.adjust(p_value, method = 'bonferroni')


# perm1 

p_value = c(0.322, 0.431, 0.826, 5e-4, 0.190, 0.00396)

p.adjust(p_value, method = 'BH')



# mcnemr 2

p_value = c(0.4531, 0.625, 0.00195, 0.03906, 0.5, 0.125)

p.adjust(p_value, method = 'BH')

# perm2

p_value = c(0.017, 0.036, 0.00128, 0,  0.0021, 0.020)

p.adjust(p_value, method = 'BH')


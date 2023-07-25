# example data ------------------------------------------------------------
n_col <- 3

o_table <-  matrix(data = c(10,20,30,20,30,50,10,20,10),ncol = n_col)

o_table

# degrees_of_freedom 
dof <- (ncol(o_table)-1) * (nrow(o_table)-1)
dof

row_marginal <- apply(X = o_table,MARGIN = 1,FUN = sum)
row_marginal

col_marginal <- apply(X = o_table,MARGIN = 2,FUN = sum)
col_marginal

n <- sum(o_table)
n

# example cell 1 ----------------------------------------------------------

o_cell1 <- o_table[1,1]
o_cell1

rm_cell1 <- row_marginal[1]
rm_cell1

cm_cell1 <- col_marginal[1]
cm_cell1

e_cell1 <- rm_cell1 * cm_cell1/n
e_cell1


# example statistic -------------------------------------------------------


e_col1 <- round(col_marginal[1] * row_marginal /n,2)
e_col2 <- round(col_marginal[2] * row_marginal /n,2)
e_col3 <- round(col_marginal[3] * row_marginal /n,2)

e_table <- round(matrix(data = c(e_col1,e_col2,e_col3),ncol = 3),2)
e_table

chi_table <- round((o_table-e_table)^2/e_table,2)
chi_table

chi_square <- sum(chi_table)
chi_square

p_value <- chisq.test(o_table)$p.value
p_value

pchisq(chi_square,df = 4,lower.tail = FALSE)


# alternative chi-square --------------------------------------------------

sum((o_table^2)/e_table)-n

# example plot ------------------------------------------------------------

library(ggplot2)
library(relper)
library(dplyr)

ggplot(data.frame(x = c(-0, 18))) +
  stat_function(mapping = aes(x = x),fun = dchisq, args = list(df =4),size = .8)+
  plt_theme_x(base_size = 16)+
  labs(
    x = expression(X**2),
    y = "",
    # subtitle = "",
    title = expression(paste(chi[4]**2," density distribution.")),
    caption = "Source: @vbfelix."
  )+
  flip_y_title+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0),breaks = seq(0,20,2), sec.axis = sec_axis(~.,breaks = chi_square,labels = chi_square))+
  geom_vline(xintercept = chi_square, linetype = "dashed", col = 'red', size = .75)+
  geom_area(
    data = data.frame(x = seq(chi_square,18,l= 100)) %>% mutate(y = dchisq(x,dof)),
    aes(x,y),
    fill = "red",
    alpha = .5
  )+
  annotate(geom = "text",x = 12,y = .035,label = paste0("p value = ",format_p_value(p_value)), size = 5)



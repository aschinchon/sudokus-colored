# Load in libraries
library(gtools)
library(dplyr)
library(sudoku)
library(ggplot2)
library(colourlovers)

# To create all possible permutations of subsquent sudokus, I start generating all
# permutations of three elements (6 in total)
# I create column k to do cross joins later
permutations(n=3, r=3, v=1:3) %>% 
  as.data.frame() %>% 
  mutate(k=1) -> perm

# Cross join permutations 4 times. I will use last three columns to interchange
# groups of columns and first nine to interchange within columns
perm %>% 
  inner_join(perm, by="k") %>% 
  inner_join(perm, by="k") %>% 
  inner_join(perm, by="k") %>% 
  select(-k) -> total_all

# Rename columns to make it understable
colnames(total_all)=c(paste0("v",1:9),"c1","c2","c3")

# Generate all possibilities. Each row of total_all is a possible reordering
# of columns to create a new sudoku
total_all %>% 
  mutate_at(vars(v1:v3), function(.) .+3*total_all[,"c1"]-3) %>% 
  mutate_at(vars(v4:v6), function(.) .+3*total_all[,"c2"]-3) %>% 
  mutate_at(vars(v7:v9), function(.) .+3*total_all[,"c3"]-3) %>% 
  select(v1:v9)-> total_all

n=3 # n can take value from 1 to 9
data <- data.frame()
total=total_all

# This loop generates n disjoint sudokus from a base one (when i is equal to 1) 
# and resampling from total_all
for (i in n:1)
{
  if(i==n)
  {
    sudoku <- generateSudoku(0)
    data.frame(level=sudoku %>% as.vector(), size=i)->new
    compare=apply(total, 1, function(x) sum(abs(x-(1:9))==0))
  } else
  {
    total %>% sample_n(1) -> tran
    sudoku_trans <- sudoku[tran %>% as.numeric,]
    data.frame(level=sudoku_trans %>% as.vector(), size=i)->new
    compare=apply(total, 1, function(x) sum(abs(x-as.numeric(tran))==0))
  }
  expand.grid(x=1:9, y=1:9) %>% cbind(new) %>% rbind(data) -> data
  total=total[which(compare==0),] #  To guaranteeing subsequent sudokus are disjoint from previous
  
}

# Pick a palette from colourLovers
# My favourite are: "1930", "482774", "694737", "953498" y "292482"
id="77121"
id="443995"
id="49963"
id="953498"
palette <- clpalette(id) %>% swatch %>% .[[1]] %>% unique() %>% colorRampPalette()

# Do the plot
plot <- ggplot(data, aes(group=size, fill=level)) + 
  geom_rect(aes(xmin=x-((n-(size-1))/n)*0.5, 
                xmax=x+((n-(size-1))/n)*0.5, 
                ymin=y-((n-(size-1))/n)*0.5, 
                ymax=y+((n-(size-1))/n)*0.5))+
  scale_fill_gradientn(colors=palette(9)) + 
  coord_fixed()+
  theme_void()+
  theme(legend.position="none")

# To see the plot on screen
plot

# Do you like it? Save it!
ggsave("my_tridoku.png", height=4, width=4, units='in', dpi=600)

"""
Uses an array to back a grid on-screen
"""

#import Pygame
import pygame 

#Colors!!!!
BLACK = (0,0,0)
WHITE = (255,255,255)
GREEN = (0,255,0)
RED = (255,0,0)

#set width and hight of each grid location 
width = 15
height = 15


#sets the margin between each cell
margin = 1

#nodes experiment
all_nodes = []
for x in range(56):
	for y in range(56):
		all_nodes.append([x,y])

'''
#check for neighboring nodes 
def neighbors(node):
	dirs = [[1,0], [0,1], [-1,0], [0,-1], [1,1], [-1,1], [-1,-1], [1,-1]]
	result = []
	for dir in dirs:
		result.append([node[0] + dir[0], node[1] + dir[1]])
	return result
'''

#alternivly, we can check to see if the coordinates are in range 
def neighbors(node):
	dirs = [[1,0], [0,1], [-1,0], [0,-1], [1,1], [-1,1], [-1,-1], [1,-1]]
	result = []
	for dir in dirs:
		neighbor = [node[0] +dir[0], node[1] + dir[1]]
		if 0 <= neighbor[0] < 25 and 0 <= neighbor[1] < 36:
			result.append(neighbor)



#Create a 2 dimensional array. (lists of lists)
grid = []
for row in range(56):
	# Add an empty array that will hold each cell in this row
	grid.append([])
	for column in range(56): # fix
		grid[row].append(0) # append a cell

# List of clicked points 
row_clicked = []
col_clicked = []
# Number of Clicked points 
num_clicked = 0 
#initiate pygame
pygame.init()

#clock 
clock = pygame.time.Clock()

#set the hight and width of the screen 
SIZE = [900,900]
screen = pygame.display.set_mode(SIZE)

pygame.display.set_caption("Array Backed Grid")

#Loop until the user click the close button
done = False 

# GAME LOOP 
while done == False:
	for event in pygame.event.get():
		if event.type == pygame.QUIT:
			done = True
		elif event.type == pygame.MOUSEBUTTONDOWN:
			#get mouseclick position
			pos = pygame.mouse.get_pos()
			# change the x/y screen coordinates to grid coordinates 
			column = pos[0] //  (width + margin)
			row = pos[1] // (height + margin)
			#set that location to zero
			if num_clicked < 35:
				grid[row][column] = 1
				row_clicked.append(str(55 - row))
				col_clicked.append(str(column))
				num_clicked += 1 
				print ("Click", column, 55 - row, "Num Clicked", num_clicked)

	#fill screen 
	screen.fill(BLACK)

	#draw the grid 
	for row in range(56):
		for column in range(56):
			color = WHITE
			if grid[row][column] == 1:
				color = GREEN
			pygame.draw.rect(screen, color, [(margin +width)*column+margin, (margin + height)* row +margin, width, height])

	#clock 
	clock.tick(60)

	pygame.display.flip()

vis_sim = open('vis_sim_R1.txt', 'w')

for item in row_clicked:
	vis_sim.write(item)
	vis_sim.write("\n")
vis_sim.close()

vis_sim2 = open('vis_sim_V2.txt', 'w')
for item in col_clicked:
	vis_sim2.write(item)
	vis_sim2.write("\n")
vis_sim2.close()


pygame.quit()




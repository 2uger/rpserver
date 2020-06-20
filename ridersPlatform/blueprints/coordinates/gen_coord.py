from random import randint
import time


def random_riders_coord(n):
	riders_list = []
	for i in range(n):
		c = []
		c.append(randint(0, 180))
		c.append(randint(0, 360))
		c.append(randint(0,9))
		c.append(randint(0,3))
		riders_list.append(c)
	return riders_list


def random_moves(riders_list):
	print(riders_list)
	for x in riders_list:
		if x[2] % 10 == 0:
			x[3] = randint(0,3)
		else:
			moves_list[x[3]](x)
		x[2] += 1
	return riders_list 


def check_out(func):
	def wrapper(m):
		if m[0] > 0 and m[1] > 0:
			func(m)
		else:
			return m	
	return wrapper
	
@check_out
def random_moves_1(c):
	c[0] += 0.0001
	c[1] += 0.0001
	return c

@check_out
def random_moves_2(c):
	c[0] -= 0.0001
	c[1] += 0.0001
	return c

@check_out
def random_moves_3(c):
	c[0] += 0.0001
	c[1] -= 0.0001
	return c

@check_out
def random_moves_4(c):
	c[0] -= 0.0001
	c[1] -= 0.0001
	return c


def change_coord():
	riders_list = random_riders_coord(3)
	while True:
		random_moves(riders_list)
		time.sleep(1)
		print(riders_list)	

moves_list = [random_moves_1, random_moves_2, random_moves_3, random_moves_4] 

if __name__ == '__main__':
	change_coord()

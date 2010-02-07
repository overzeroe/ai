# This program plays DropZap at http://dropzap.appspot.com
# It uses AutoPy, which is an absolutely brilliant library at http://github.com/msanders/autopy
# It uses heuristics to evaluate how good all possible moves are, then makes the move

# Import automation
from autopy import *
from time import sleep

# Find game board on screen
game_board = bitmap.Bitmap.open('board_top.png')
pos = bitmap.capture_screen().find_bitmap(game_board)
EDGE_TOP = pos[1]
EDGE_LEFT = pos[0]

# Create board 
board = [[0]*7 for x in range(7)]

# Calculate block positions
WIDTH = 308/7
HEIGHT = 308/7
row_pos = [EDGE_LEFT + x for x in [21, 65, 110, 152, 197, 242, 285]]
positions = [zip(row_pos, [EDGE_TOP + HEIGHT/2 + HEIGHT * 0] * 7),
             zip(row_pos, [EDGE_TOP + HEIGHT/2 + HEIGHT * 1] * 7),
             zip(row_pos, [EDGE_TOP + HEIGHT/2 + HEIGHT * 2] * 7),
             zip(row_pos, [EDGE_TOP + HEIGHT/2 + HEIGHT * 3] * 7),
             zip(row_pos, [EDGE_TOP + HEIGHT/2 + HEIGHT * 4] * 7),
             zip(row_pos, [EDGE_TOP + HEIGHT/2 + HEIGHT * 5] * 7),
             zip(row_pos, [EDGE_TOP + HEIGHT/2 + HEIGHT * 6] * 7)]

# Move our mouse
def move(x, y): 
    global positions
    pos = positions[x][y]
    mouse.move(pos[0], pos[1])

# Check whether the move is done
def moving():
    color = find_next_square()
    return color == '0x0'

# Wait until the move is completed
# The screenshot takes a long time, so it's annoying to watch
def finish_move():
    if moving():
        finish_move()

# Map colors to square types
def square(color):
    colors = ['0x0', '0xff0000', '0xff8000', '0xffff00', '0xff00', '0xffff', '0xff']
    if hex(color) in colors:
        return colors.index(hex(color))

    return None

# Retrieve the board
def read_board():
    global board

    bitmp = bitmap.capture_screen()

    # Look at each block
    for x in range(7):
        for y in range(7):
            pos = positions[x][y]
            board[x][y] = square(bitmp.get_color(pos[0], pos[1]))
            
            # For debugging, move mouse to place if it fails
            if board[x][y] == None:
                move(x, y)
                sleep(.1)
                bitmp.save("save.bmp")
                raise ValueError()

    return board

# What color will the next square be? (Look to the top.)
def find_next_square():
    return square(bitmap.capture_screen().get_color(EDGE_LEFT + 154, EDGE_TOP - 25))

# Initiate the move (do the clicking)
def select_column(col):
    move(6, col)
    mouse.click()
    sleep(3) # wait a bit for smoother animation

# Choose and make a move
def next_move():
    global board

    # Get our data
    read_board()

    # Get the seven possible future boards
    potential = calc_future_boards(board)

    # Calculate our heuristic for each of the boards
    heuristics = [calc_heuristic(future) for future in potential]

    # Choose the best move and make it
    col = heuristics.index(max(heuristics))
    select_column(col)

    # Wait for the move to end
    finish_move()

# Calculate all future boards for the heuristic to evaluate
def calc_future_boards(curr):
    next_square = find_next_square()
    return [future_board(curr, col_move, next_square) for col_move in range(7)]

# Calculate the board if we go to the given column
def future_board(current, col_move, next_square):

    # Make sure we can put something in this column
    if current[0][col_move] != 0: return None

    # Create a copy of the board to work with
    board_copy = [orig_row[:] for orig_row in current]

    # Place the new square (it will drop on its own when the board is state-shifted)
    board_copy[0][col_move] = next_square

    # An annotation field to store stuff about how the shifting went
    board_copy.append({'dec':0, 'steps':0})

    # Keep updating the board while there are updates to do
    while(board_state_shift(board_copy)): pass

    return board_copy

# Update state of the board according to the game's laws of physics
def board_state_shift(brd):
    # Keep track of whether things have changed
    changed = False

    # Shift all hovering squares down, while storing all the places things landed
    # Places landed
    landed = []

    # Shift
    for col in range(7):
        for row in range(6)[::-1]:
            # If a slot is black but above it isn't, we must shift
            if row > 0 and brd[row][col] == 0 and brd[row - 1][col] != 0:
                landed.append((col, row))
                changed = True

                for move_row in range(0,row + 1)[::-1]:
                    if move_row == 0:
                        brd[move_row][col] = 0
                    else:
                        brd[move_row][col] = brd[move_row - 1][col]


    # Commence explosions as dictated by DropZap Physics Engine (TM)
    for landing in landed:
        landed_row = landing[1]
        landed_col = landing[0]

        # Move left, right, and down from the landed thing, decreasing counts by one
        exploded = False
        # Left
        for col in range(landed_col)[::-1]:
            if brd[landed_row][col] == 0: 
                break
            exploded = True
            brd[7]['dec'] += 1
            brd[landed_row][col] -= 1

        # Right
        for col in range(landed_col, 7):
            if brd[landed_row][col] == 0:
                break
            exploded = True
            brd[7]['dec'] += 1
            brd[landed_row][col] -= 1

        # Down
        for row in range(landed_row + 1, 7):
            exploded = True
            brd[7]['dec'] += 1
            brd[row][landed_col] -= 1

        if exploded: brd[7]['steps'] += 1

    return changed

# Define a heuristic to evaluate "goodness" of a board
def calc_heuristic(brd):
    # If a board is impossible, it's None; we don't want these
    if brd is None: 
        return -99999999999999L

    # We secretly added an annotation to store data stuff
    annotation = brd[7]
    brd.remove(annotation)

    # The more we destroy in a turn the better
    destroy_count = annotation['dec']

    # The more steps of destruction there are the better
    step_count = annotation['steps'] * 10

    # But if squares are left over that's bad. Penalize!
    square_count = 0
    for row in brd:
        for x in row:
            if x != 0: square_count += 1

    # Penalize for having stuff up high
    penalty = 0
    for sq in brd[0]:
        if sq != 0: 
            penalty += 9999999999L

    return destroy_count + step_count - square_count - penalty


# Check whether we have lost 
def have_lost():
    # There's an annoying white popup
    return '0xffffff' == hex(bitmap.capture_screen().get_color(725, 500))


def run():
    try:
        # Run until we're dead
        while not have_lost():
            next_move()
    except ValueError:
        run()


# Go!
run()

package unit;

import mars.constants.CardinalDirections;
import mars.interfaces.IMovementEnforcer;
import mars.interfaces.IOrientationEnforcer;
import mars.valueobjects.GridLocation;
import mars.enforcers.MarsMovementEnforcer;
import mars.enforcers.MarsOrientationEnforcer;
import mars.exceptions.DisallowedRoverMovementException;
import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Created by michaelnyika on 11/8/14.
 *
 * These tests are responsible for making sure that the rover moves a single step in the correct
 * direction. First we need to find out what direction we are facing, since the direction will
 * always determine if we need to augment or subtract from the value of the X position and/or
 * the Y position. Like orientation, we will make certain assumptions as initial conditions.
 * Since only the M instruction is possible for movement, we will only concentrate on that
 *
 * * //Assumptions:
 * 1. That we have a simple 3x3 grid where at least 1 positional step can be taken in any direction
 * 2. That orientations in any of the 4 cardinal directions are possible
 * 3. That at the start of each NON-BORDER-CASE-test, we are always in the center of the grid (or 11N)
 * 4. L = LEFT, R = RIGHT, M = MOVE
 */
public class RoverMovementTests {

    private GridLocation gridLocation;
    private IOrientationEnforcer marsOrientationEnforcer;
    private static int original_x_value;
    private static int original_y_value;
    private IMovementEnforcer marsMovementEnforcer;

    @BeforeClass
    public static void before_we_begin_all_tests(){
        original_x_value = 1;
        original_y_value = 1;
    }


    @Before
    public void before_we_begin_any_test()
    {
        marsOrientationEnforcer = new MarsOrientationEnforcer();
        gridLocation = new GridLocation();

        gridLocation.setCurrent_X_position(original_x_value);
        gridLocation.setCurrent_Y_position(original_y_value);
        gridLocation.setCurrent_CardinalOrientation("N");

        marsMovementEnforcer = new MarsMovementEnforcer();
    }

    @Test
    public void given_oriented_NORTH_and_position_1_1_then_move_to_1_2_check_X() throws Exception{
        GridLocation finalLocation = marsMovementEnforcer.moveRover(gridLocation);
        Assert.assertEquals(1, finalLocation.getCurrent_X_position());
    }

    @Test
    public void given_oriented_NORTH_and_position_1_1_then_move_to_1_2_check_Y() throws Exception{
        GridLocation finalLocation = marsMovementEnforcer.moveRover(gridLocation);
        Assert.assertEquals(2, finalLocation.getCurrent_Y_position());
    }

    @Test(expected=DisallowedRoverMovementException.class)
    public void given_oriented_NORTH_and_position_0_9_then_movement_not_possible() throws Exception{
        gridLocation.setCurrent_X_position(0);
        gridLocation.setCurrent_Y_position(9);
        marsMovementEnforcer.moveRover(gridLocation);
    }

    @Test
    public void given_oriented_EAST_and_position_1_1_then_move_to_2_1_check_X() throws Exception{
        gridLocation.setCurrent_CardinalOrientation(CardinalDirections.EAST.DIR());
        GridLocation finalLocation = marsMovementEnforcer.moveRover(gridLocation);
        Assert.assertEquals(2, finalLocation.getCurrent_X_position());
    }

    @Test
    public void given_oriented_EAST_and_position_1_1_then_move_to_2_1_check_Y() throws Exception{
        gridLocation.setCurrent_CardinalOrientation(CardinalDirections.EAST.DIR());
        GridLocation finalLocation = marsMovementEnforcer.moveRover(gridLocation);
        Assert.assertEquals(1, finalLocation.getCurrent_Y_position());
      }

    @Test(expected=DisallowedRoverMovementException.class)
    public void given_oriented_EAST_and_position_9_9_then_movement_not_possible() throws Exception{
        gridLocation.setCurrent_CardinalOrientation(CardinalDirections.EAST.DIR());
        gridLocation.setCurrent_X_position(9);
        gridLocation.setCurrent_Y_position(9);
        marsMovementEnforcer.moveRover(gridLocation);
    }

    @Test
    public void given_oriented_SOUTH_and_position_1_1_then_move_to_1_0_check_X() throws Exception{
        gridLocation.setCurrent_CardinalOrientation(CardinalDirections.SOUTH.DIR());
        GridLocation finalLocation = marsMovementEnforcer.moveRover(gridLocation);
        Assert.assertEquals(1, finalLocation.getCurrent_X_position());
      }

    @Test
    public void given_oriented_SOUTH_and_position_1_1_then_move_to_1_0_check_Y() throws Exception{
        gridLocation.setCurrent_CardinalOrientation(CardinalDirections.SOUTH.DIR());
        GridLocation finalLocation = marsMovementEnforcer.moveRover(gridLocation);
        Assert.assertEquals(0, finalLocation.getCurrent_Y_position());
    }

    @Test(expected=DisallowedRoverMovementException.class)
    public void given_oriented_SOUTH_and_position_9_0_then_movement_not_possible() throws Exception{
        gridLocation.setCurrent_CardinalOrientation(CardinalDirections.SOUTH.DIR());
        gridLocation.setCurrent_X_position(9);
        gridLocation.setCurrent_Y_position(0);
        marsMovementEnforcer.moveRover(gridLocation);
    }

    @Test
    public void given_oriented_WEST_and_position_1_1_then_move_to_0_1_check_X() throws Exception{
        gridLocation.setCurrent_CardinalOrientation(CardinalDirections.WEST.DIR());
        GridLocation finalLocation = marsMovementEnforcer.moveRover(gridLocation);
        Assert.assertEquals(0, finalLocation.getCurrent_X_position());
       }

    @Test
    public void given_oriented_WEST_and_position_1_1_then_move_to_0_1_check_Y() throws Exception{
        gridLocation.setCurrent_CardinalOrientation(CardinalDirections.WEST.DIR());
        GridLocation finalLocation = marsMovementEnforcer.moveRover(gridLocation);
        Assert.assertEquals(1, finalLocation.getCurrent_Y_position());
        }

    @Test(expected=DisallowedRoverMovementException.class)
    public void given_oriented_WEST_and_position_0_0_then_movement_not_possible() throws Exception{
        gridLocation.setCurrent_CardinalOrientation(CardinalDirections.WEST.DIR());
        gridLocation.setCurrent_X_position(0);
        gridLocation.setCurrent_Y_position(0);
        marsMovementEnforcer.moveRover(gridLocation);
    }

    @Test
    public void given_orientation_UNKNOWN_and_position_0_0_then_rover_is_unmoved_horizontally() throws Exception{
        String unusual_direction = "Pointing-Nowhere";
        gridLocation.setCurrent_CardinalOrientation(unusual_direction);
        gridLocation.setCurrent_X_position(0);
        gridLocation.setCurrent_Y_position(0);
        GridLocation alteredLocation = marsMovementEnforcer.moveRover(gridLocation);
        Assert.assertEquals(alteredLocation.getCurrent_X_position(), gridLocation.getCurrent_X_position());
    }

    @Test
    public void given_orientation_UNKNOWN_and_position_0_0_then_rover_is_unmoved_vertically() throws Exception{
        String unusual_direction = "Pointing-Nowhere";
        gridLocation.setCurrent_CardinalOrientation(unusual_direction);
        gridLocation.setCurrent_X_position(0);
        gridLocation.setCurrent_Y_position(0);
        GridLocation alteredLocation = marsMovementEnforcer.moveRover(gridLocation);
        Assert.assertEquals(alteredLocation.getCurrent_Y_position(), gridLocation.getCurrent_Y_position());
    }


    //NOW TESTS TO CHECK THAT YOUR ORIENTATION DOES NOT CHANGE WHEN YOU PHYSICALLY MOVE

    @Test
    public void given_oriented_NORTH_and_position_1_1_and_move_then_orientation_is_untouched() throws Exception{
        GridLocation finalLocation = marsMovementEnforcer.moveRover(gridLocation);
        Assert.assertEquals(CardinalDirections.NORTH.DIR(), finalLocation.getCurrent_CardinalOrientation());
    }

    @Test
    public void given_oriented_EAST_and_position_1_1_and_move_to_then_orientation_is_same() throws Exception{
        gridLocation.setCurrent_CardinalOrientation(CardinalDirections.EAST.DIR());
        GridLocation finalLocation = marsMovementEnforcer.moveRover(gridLocation);
        Assert.assertEquals(CardinalDirections.EAST.DIR(), finalLocation.getCurrent_CardinalOrientation());
  }

    @Test
    public void given_oriented_SOUTH_and_position_1_1_and_move_then_orientation_does_not_change() throws Exception{
        gridLocation.setCurrent_CardinalOrientation(CardinalDirections.SOUTH.DIR());
        GridLocation finalLocation = marsMovementEnforcer.moveRover(gridLocation);
        Assert.assertEquals(CardinalDirections.SOUTH.DIR(), finalLocation.getCurrent_CardinalOrientation());
    }

    @Test
    public void given_oriented_WEST_and_position_1_1_and_move_then_orientation_is_static() throws Exception{
        gridLocation.setCurrent_CardinalOrientation(CardinalDirections.WEST.DIR());
        GridLocation finalLocation = marsMovementEnforcer.moveRover(gridLocation);
        Assert.assertEquals(CardinalDirections.WEST.DIR(), finalLocation.getCurrent_CardinalOrientation());
    }

}

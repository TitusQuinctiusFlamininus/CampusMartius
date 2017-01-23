package unit;

import mars.constants.CardinalDirections;
import mars.interfaces.IOrientationEnforcer;
import mars.valueobjects.GridLocation;
import mars.enforcers.MarsOrientationEnforcer;
import mars.constants.OrientationInstruction;
import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Created by michaelnyika on 11/8/14.
 *
 * These unit tests check that useless orientation instructions
 * are simply ignored and not intrusive on the execution of
 * instructions later in the instruction-sequence
 *
 */
public class RoverOrientationTests {

    private GridLocation gridLocation;
    private IOrientationEnforcer marsOrientationEnforcer;
    private static int original_x_value;
    private static int original_y_value;

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
    }
    //Let us first check that final orientation direction can be correctly determined
    //given an initial orientation

    @Test
    public void given_orientation_instruction_is_not_left_or_right_and_initial_orient_is_NORTH_then_skip_orientation_exercise(){
        marsOrientationEnforcer.orientRover(gridLocation, "U");
        Assert.assertEquals(CardinalDirections.NORTH.DIR(),gridLocation.getCurrent_CardinalOrientation());
    }

    @Test
    public void given_orientation_instruction_is_not_left_or_right_and_initial_orient_is_SOUTH_then_skip_orientation_exercise(){
        gridLocation.setCurrent_CardinalOrientation(CardinalDirections.SOUTH.DIR());
        marsOrientationEnforcer.orientRover(gridLocation, "V");
        Assert.assertEquals(CardinalDirections.SOUTH.DIR(),gridLocation.getCurrent_CardinalOrientation());
    }

    @Test
    public void given_orientation_instruction_is_not_left_or_right_and_initial_orient_is_EAST_then_skip_orientation_exercise(){
        gridLocation.setCurrent_CardinalOrientation(CardinalDirections.EAST.DIR());
        marsOrientationEnforcer.orientRover(gridLocation, "G");
        Assert.assertEquals(CardinalDirections.EAST.DIR(),gridLocation.getCurrent_CardinalOrientation());
    }

    @Test
    public void given_orientation_instruction_is_not_left_or_right_and_initial_orient_is_WEST_then_skip_orientation_exercise(){
        gridLocation.setCurrent_CardinalOrientation(CardinalDirections.WEST.DIR());
        marsOrientationEnforcer.orientRover(gridLocation, "G");
        Assert.assertEquals(CardinalDirections.WEST.DIR(),gridLocation.getCurrent_CardinalOrientation());
    }


}

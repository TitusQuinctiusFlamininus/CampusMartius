package unit;

import mars.constants.CardinalDirections;
import mars.interfaces.IOrientationEnforcer;
import mars.valueobjects.GridLocation;
import mars.enforcers.MarsOrientationEnforcer;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 * Created by michaelnyika on 11/8/14.
 * These tests check that the MovementEnforcer makes the
 * correct decisions regarding which map to use for final orientation
 * mapping, since the initial conditions of the rover at each stage
 * of start of movement could be different (i.e the rover could be facing one
 * of 4 directions at any time before any instructions are executed)
 *
 */

public class OrientationMapSelectorTests {

    private MarsOrientationEnforcer orientationEnforcer;
    private GridLocation someLocation;

    @Before
    public void at_the_beginning()
    {
        orientationEnforcer = new MarsOrientationEnforcer();
        someLocation = new GridLocation();
    }

    @Test
    public void given_current_orientation_is_N_then_use_the_correct_Directional_mapper(){
        someLocation.setCurrent_CardinalOrientation(CardinalDirections.NORTH.DIR());
        Assert.assertEquals("NORTH_FACING", orientationEnforcer.find_Correct_Direction_Map(someLocation).get("NAME"));
    }

    @Test
    public void given_current_orientation_is_E_then_use_the_correct_Directional_mapper(){
        someLocation.setCurrent_CardinalOrientation(CardinalDirections.EAST.DIR());
        Assert.assertEquals("EAST_FACING", orientationEnforcer.find_Correct_Direction_Map(someLocation).get("NAME"));
    }

    @Test
    public void given_current_orientation_is_S_then_use_the_correct_Directional_mapper(){
        someLocation.setCurrent_CardinalOrientation(CardinalDirections.SOUTH.DIR());
        Assert.assertEquals("SOUTH_FACING", orientationEnforcer.find_Correct_Direction_Map(someLocation).get("NAME"));
    }

    @Test
    public void given_current_orientation_is_W_then_use_the_correct_Directional_mapper(){
        someLocation.setCurrent_CardinalOrientation(CardinalDirections.WEST.DIR());
        Assert.assertEquals("WEST_FACING", orientationEnforcer.find_Correct_Direction_Map(someLocation).get("NAME"));
    }
}

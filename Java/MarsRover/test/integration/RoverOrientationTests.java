package integration;

import mars.constants.CardinalDirections;
import mars.constants.OrientationInstruction;
import mars.enforcers.MarsOrientationEnforcer;
import mars.interfaces.IOrientationEnforcer;
import mars.valueobjects.GridLocation;
import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Created by michaelnyika on 11/9/14.
 *
 * These tests are also for the orientation of the rover
 * by since the execution path involves multiple methods within
 * the same testSubject (the MarsOrientationEnforcer), they
 * are integration tests
 *
 *  * These tests are pretty important, since they make sure that any instruction
 * properly carried, given an current position, will result in an end-position
 * that has the correct from the absolute grid positions; also, that the orientation
 * is correct in the end state
 *
 * //Assumptions:
 * 1. That we have a simple 3x3 grid where at least 1 positional step can be taken in any direction
 * 2. That orientations in any of the 4 cardinal directions are possible
 * 3. That at the start of each test, we are always in the center of the grid (or 11N)
 * 4. L = LEFT, R = RIGHT, M = MOVE

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

    //TESTS THAT CHECK THAT ORIENTATION OF THE ROVER OCCURS CORRECTLY ONCE IT IS GIVEN AN INSTRUCTION

    @Test
    public void given_initial_direction_NORTH_and_instruction_LEFT_then_final_direction_is_W(){
        String instruction = OrientationInstruction.LEFT.way();
        Assert.assertEquals(CardinalDirections.WEST.DIR(), marsOrientationEnforcer.orientRover(gridLocation, instruction).getCurrent_CardinalOrientation());
    }

    @Test
    public void given_initial_direction_NORTH_and_instruction_RIGHT_then_final_direction_is_E(){
        String instruction = OrientationInstruction.RIGHT.way();
        Assert.assertEquals(CardinalDirections.EAST.DIR(), marsOrientationEnforcer.orientRover(gridLocation, instruction).getCurrent_CardinalOrientation());
    }

    @Test
    public void given_initial_direction_EAST_and_instruction_RIGHT_then_final_direction_is_S(){
        gridLocation.setCurrent_CardinalOrientation(CardinalDirections.EAST.DIR());
        String instruction = OrientationInstruction.RIGHT.way();
        Assert.assertEquals(CardinalDirections.SOUTH.DIR(), marsOrientationEnforcer.orientRover(gridLocation, instruction).getCurrent_CardinalOrientation());
    }

    @Test
    public void given_initial_direction_EAST_and_instruction_LEFT_then_final_direction_is_N(){
        gridLocation.setCurrent_CardinalOrientation(CardinalDirections.EAST.DIR());
        String instruction = OrientationInstruction.LEFT.way();
        Assert.assertEquals(CardinalDirections.NORTH.DIR(), marsOrientationEnforcer.orientRover(gridLocation, instruction).getCurrent_CardinalOrientation());
    }

    @Test
    public void given_initial_direction_SOUTH_and_instruction_LEFT_then_final_direction_is_E(){
        gridLocation.setCurrent_CardinalOrientation(CardinalDirections.SOUTH.DIR());
        String instruction = OrientationInstruction.LEFT.way();
        Assert.assertEquals(CardinalDirections.EAST.DIR(), marsOrientationEnforcer.orientRover(gridLocation, instruction).getCurrent_CardinalOrientation());
    }

    @Test
    public void given_initial_direction_SOUTH_and_instruction_RIGHT_then_final_direction_is_W(){
        gridLocation.setCurrent_CardinalOrientation(CardinalDirections.SOUTH.DIR());
        String instruction = OrientationInstruction.RIGHT.way();
        Assert.assertEquals(CardinalDirections.WEST.DIR(), marsOrientationEnforcer.orientRover(gridLocation, instruction).getCurrent_CardinalOrientation());
    }

    @Test
    public void given_initial_direction_WEST_and_instruction_LEFT_then_final_direction_is_S(){
        gridLocation.setCurrent_CardinalOrientation(CardinalDirections.WEST.DIR());
        String instruction = OrientationInstruction.LEFT.way();
        Assert.assertEquals(CardinalDirections.SOUTH.DIR(), marsOrientationEnforcer.orientRover(gridLocation, instruction).getCurrent_CardinalOrientation());
    }

    @Test
    public void given_initial_direction_WEST_and_instruction_RIGHT_then_final_direction_is_N(){
        gridLocation.setCurrent_CardinalOrientation(CardinalDirections.WEST.DIR());
        String instruction = OrientationInstruction.RIGHT.way();
        Assert.assertEquals(CardinalDirections.NORTH.DIR(), marsOrientationEnforcer.orientRover(gridLocation, instruction).getCurrent_CardinalOrientation());
    }


    //TESTS THAT CHECK THAT X and Y POSITION OF THE ROVER DOES NOT CHANGE
    // ONCE IT IS GIVEN (and executes) AN ORIENTATION INSTRUCTION

    @Test
    public void given_initial_direction_NORTH_and_instruction_LEFT_then_final_X_position_same_as_initial_position(){
        String instruction = OrientationInstruction.LEFT.way();
        GridLocation finalLocation = marsOrientationEnforcer.orientRover(gridLocation, instruction);
        check_position_X_did_not_change(finalLocation);
    }

    @Test
    public void given_initial_direction_NORTH_and_instruction_LEFT_then_final_Y_position_same_as_initial_position(){
        String instruction = OrientationInstruction.LEFT.way();
        GridLocation finalLocation = marsOrientationEnforcer.orientRover(gridLocation, instruction);
        check_position_Y_did_not_change(finalLocation);
    }

    @Test
    public void given_initial_direction_NORTH_and_instruction_RIGHT_then_final_X_position_same_as_initial_position(){
        String instruction = OrientationInstruction.RIGHT.way();
        GridLocation finalLocation = marsOrientationEnforcer.orientRover(gridLocation, instruction);
        check_position_X_did_not_change(finalLocation);
    }

    @Test
    public void given_initial_direction_NORTH_and_instruction_RIGHT_then_final_Y_position_same_as_initial_position(){
        String instruction = OrientationInstruction.RIGHT.way();
        GridLocation finalLocation = marsOrientationEnforcer.orientRover(gridLocation, instruction);
        check_position_Y_did_not_change(finalLocation);
    }

    @Test
    public void given_initial_direction_EAST_and_instruction_RIGHT_then_final_X_position_same_as_initial_position(){
        gridLocation.setCurrent_CardinalOrientation(CardinalDirections.EAST.DIR());
        String instruction = OrientationInstruction.RIGHT.way();
        GridLocation finalLocation = marsOrientationEnforcer.orientRover(gridLocation, instruction);
        check_position_X_did_not_change(finalLocation);
    }

    @Test
    public void given_initial_direction_EAST_and_instruction_RIGHT_then_final_Y_position_same_as_initial_position(){
        gridLocation.setCurrent_CardinalOrientation(CardinalDirections.EAST.DIR());
        String instruction = OrientationInstruction.RIGHT.way();
        GridLocation finalLocation = marsOrientationEnforcer.orientRover(gridLocation, instruction);
        check_position_Y_did_not_change(finalLocation);
    }

    @Test
    public void given_initial_direction_EAST_and_instruction_LEFT_then_final_X_position_same_as_initial_position(){
        gridLocation.setCurrent_CardinalOrientation(CardinalDirections.EAST.DIR());
        String instruction = OrientationInstruction.LEFT.way();
        GridLocation finalLocation = marsOrientationEnforcer.orientRover(gridLocation, instruction);
        check_position_X_did_not_change(finalLocation);
    }

    @Test
    public void given_initial_direction_EAST_and_instruction_LEFT_then_final_Y_position_same_as_initial_position(){
        gridLocation.setCurrent_CardinalOrientation(CardinalDirections.EAST.DIR());
        String instruction = OrientationInstruction.LEFT.way();
        GridLocation finalLocation = marsOrientationEnforcer.orientRover(gridLocation, instruction);
        check_position_Y_did_not_change(finalLocation);
    }

    @Test
    public void given_initial_direction_SOUTH_and_instruction_LEFT_then_final_X_position_same_as_initial_position(){
        gridLocation.setCurrent_CardinalOrientation(CardinalDirections.SOUTH.DIR());
        String instruction = OrientationInstruction.LEFT.way();
        GridLocation finalLocation = marsOrientationEnforcer.orientRover(gridLocation, instruction);
        check_position_X_did_not_change(finalLocation);
    }

    @Test
    public void given_initial_direction_SOUTH_and_instruction_LEFT_then_final_Y_position_same_as_initial_position(){
        gridLocation.setCurrent_CardinalOrientation(CardinalDirections.SOUTH.DIR());
        String instruction = OrientationInstruction.LEFT.way();
        GridLocation finalLocation = marsOrientationEnforcer.orientRover(gridLocation, instruction);
        check_position_Y_did_not_change(finalLocation);
    }

    @Test
    public void given_initial_direction_SOUTH_and_instruction_RIGHT_then_final_X_position_same_as_initial_position(){
        gridLocation.setCurrent_CardinalOrientation(CardinalDirections.SOUTH.DIR());
        String instruction = OrientationInstruction.RIGHT.way();
        GridLocation finalLocation = marsOrientationEnforcer.orientRover(gridLocation, instruction);
        check_position_X_did_not_change(finalLocation);
    }

    @Test
    public void given_initial_direction_SOUTH_and_instruction_RIGHT_then_final_Y_position_same_as_initial_position(){
        gridLocation.setCurrent_CardinalOrientation(CardinalDirections.SOUTH.DIR());
        String instruction = OrientationInstruction.RIGHT.way();
        GridLocation finalLocation = marsOrientationEnforcer.orientRover(gridLocation, instruction);
        check_position_Y_did_not_change(finalLocation);
    }

    @Test
    public void given_initial_direction_WEST_and_instruction_LEFT_then_final_X_position_same_as_initial_position(){
        gridLocation.setCurrent_CardinalOrientation(CardinalDirections.WEST.DIR());
        String instruction = OrientationInstruction.LEFT.way();
        GridLocation finalLocation = marsOrientationEnforcer.orientRover(gridLocation, instruction);
        check_position_X_did_not_change(finalLocation);
    }

    @Test
    public void given_initial_direction_WEST_and_instruction_LEFT_then_final_Y_position_same_as_initial_position(){
        gridLocation.setCurrent_CardinalOrientation(CardinalDirections.WEST.DIR());
        String instruction = OrientationInstruction.LEFT.way();
        GridLocation finalLocation = marsOrientationEnforcer.orientRover(gridLocation, instruction);
        check_position_Y_did_not_change(finalLocation);
    }

    @Test
    public void given_initial_direction_WEST_and_instruction_RIGHT_then_final_X_position_same_as_initial_position(){
        gridLocation.setCurrent_CardinalOrientation(CardinalDirections.WEST.DIR());
        String instruction = OrientationInstruction.RIGHT.way();
        GridLocation finalLocation = marsOrientationEnforcer.orientRover(gridLocation, instruction);
        check_position_X_did_not_change(finalLocation);
    }

    @Test
    public void given_initial_direction_WEST_and_instruction_RIGHT_then_final_Y_position_same_as_initial_position(){
        gridLocation.setCurrent_CardinalOrientation(CardinalDirections.WEST.DIR());
        String instruction = OrientationInstruction.RIGHT.way();
        GridLocation finalLocation = marsOrientationEnforcer.orientRover(gridLocation, instruction);
        check_position_Y_did_not_change(finalLocation);
    }

    private void check_position_X_did_not_change(GridLocation finalLocation){
        Assert.assertEquals(original_x_value, finalLocation.getCurrent_X_position());
    }

    private void check_position_Y_did_not_change(GridLocation finalLocation){
        Assert.assertEquals(original_y_value, finalLocation.getCurrent_Y_position());
    }

}

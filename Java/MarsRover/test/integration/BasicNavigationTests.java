package integration;

import mars.distributors.MarsRoverPilot;
import mars.valueobjects.GridLocation;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 * Created by michaelnyika on 11/8/14.
 *
 * These tests check a few examples that could be received from NASA
 */
public class BasicNavigationTests {

    private MarsRoverPilot marsRoverPilot;

    @Before
    public void before_tarting_the_trial(){
        marsRoverPilot = new MarsRoverPilot();
    }

    @Test
    public void given_initial_position_0_0_N_then_traverse_across_entire_bottom_row_and_stop() throws Exception{
        String initialPosition = "00N";
        String navigationInstructions = "RMMMMMMMMM";

        GridLocation finalPosition = marsRoverPilot.distribute_candies(initialPosition, navigationInstructions);
        Assert.assertEquals(9,finalPosition.getCurrent_X_position());
        Assert.assertEquals(0,finalPosition.getCurrent_Y_position());
        Assert.assertEquals("E",finalPosition.getCurrent_CardinalOrientation());
    }

    @Test(expected = Exception.class)
    public void given_initial_position_5_6_W_but_no_L_R_or_M_for_instructions_then_there_is_a_problem() throws Exception{
        String initialPosition = "56W";
        String navigationInstructions = "ABCDEFGHIJKNOPQSTUVWXYZ"; //no L, R or M
        marsRoverPilot.distribute_candies(initialPosition, navigationInstructions);
    }

    @Test
    public void given_initial_position_7_9_S_and_single_L_among_strange_symbols_then_processing_occurs_normally() throws Exception{
        String initialPosition = "79S";
        String navigationInstructions = "%&/()=Ä§L@"; //single L among wierd symbols (but no R or M) : Processing will still occor
        GridLocation finalPosition = marsRoverPilot.distribute_candies(initialPosition, navigationInstructions);
        Assert.assertEquals(7,finalPosition.getCurrent_X_position());
        Assert.assertEquals(9,finalPosition.getCurrent_Y_position());
        Assert.assertEquals("E", finalPosition.getCurrent_CardinalOrientation()); //the L should turn the rover eastwards
    }

    @Test(expected = Exception.class)
    public void given_initial_position_0_0_but_no_initial_orientation_then_there_is_a_problem() throws Exception{
        String initialPosition = "00";
        String navigationInstructions = "RMMMMMMMMM";
        marsRoverPilot.distribute_candies(initialPosition, navigationInstructions);

    }


    //HERE IS THE ACTUAL REQUIREMENT FOR THIS EXERCISE AND PROOF THAT THIS PROGRAM WORKS
    @Test
    public void given_initial_position_4_4_N_then_final_should_be_2_4_E() throws Exception{
        String initialPosition = "44N";
        String navigationInstructions = "LMMMRRM";

        GridLocation finalPosition = marsRoverPilot.distribute_candies(initialPosition, navigationInstructions);
        Assert.assertEquals(2,finalPosition.getCurrent_X_position());
        Assert.assertEquals(4,finalPosition.getCurrent_Y_position());
        Assert.assertEquals("E",finalPosition.getCurrent_CardinalOrientation());
    }

    @Test
    public void given_initial_position_0_2_E_and_go_in_a_circle_once_then_final_should_be_0_2_N() throws Exception{
        String initialPosition = "02E";
        String navigationInstructions = "MMRMMRMMRMM";

        GridLocation finalPosition = marsRoverPilot.distribute_candies(initialPosition, navigationInstructions);
        Assert.assertEquals(0,finalPosition.getCurrent_X_position());
        Assert.assertEquals(2,finalPosition.getCurrent_Y_position());
        Assert.assertEquals("N",finalPosition.getCurrent_CardinalOrientation());
    }
}

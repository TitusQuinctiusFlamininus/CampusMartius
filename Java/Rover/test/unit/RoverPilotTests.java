package unit;

import mars.distributors.MarsRoverPilot;
import mars.exceptions.InitialLandingPositionException;
import mars.exceptions.MissingInitialLandingPositionException;
import mars.exceptions.MissingNavigationInputException;
import mars.exceptions.NavigationInputException;
import mars.validators.LandingPositionInputValidator;
import mars.validators.NavigationInputValidator;
import org.junit.Assert;
import org.junit.Test;

/**
 * Created by michaelnyika on 11/8/14.
 * These tests make sure that the distributor module of the rover is working
 * correctly. It is the entry point (or from a DDD perspective, it is the Root Aggregate object)
 */
public class RoverPilotTests {

    @Test(expected = MissingInitialLandingPositionException.class)
    public void given_null_initial_landing_position_then_complain_about_it() throws Exception{
        MarsRoverPilot distributor = new MarsRoverPilot();
        LandingPositionInputValidator specialValidator = new LandingPositionInputValidator(){
            public boolean validateInitialLandingPosition(String inputLandingPosition) throws InitialLandingPositionException, MissingInitialLandingPositionException {
                           throw new  MissingInitialLandingPositionException("oh no!");
                        }
                    };
        distributor.setLandingPositionValidator(specialValidator);
        distributor.distribute_candies(null, "move_this_way_then_that_way");
    }


    @Test(expected = InitialLandingPositionException.class)
    public void given_initial_landing_position_that_cannot_be_mapped_on_mars_surface_then_complain_about_it() throws Exception{
        MarsRoverPilot distributor = new MarsRoverPilot();
        LandingPositionInputValidator specialValidator = new LandingPositionInputValidator(){
            public boolean validateInitialLandingPosition(String inputLandingPosition) throws InitialLandingPositionException, MissingInitialLandingPositionException {
                throw new  InitialLandingPositionException("its just bad");
            }
        };
        distributor.setLandingPositionValidator(specialValidator);
        distributor.distribute_candies("it_exists_but_it_is_still_not_right", "move_this_way_then_that_way");
    }

    @Test(expected = MissingNavigationInputException.class)
    public void given_movement_instructions_are_null_then_complain_about_it() throws Exception{
        MarsRoverPilot distributor = new MarsRoverPilot();
        LandingPositionInputValidator specialValidator = new LandingPositionInputValidator(){
            public boolean validateInitialLandingPosition(String inputLandingPosition) throws InitialLandingPositionException, MissingInitialLandingPositionException {
                return true;
            }
        };
        NavigationInputValidator specialNaviValidator = new NavigationInputValidator(){
            public boolean validateNavigationalInput(String navigationInput) throws Exception{
                throw new MissingNavigationInputException("we received no movemnt instructions...(sigh)");
            }
        };
        distributor.setLandingPositionValidator(specialValidator);
        distributor.setNavigationValidator(specialNaviValidator);
        distributor.distribute_candies("great_landing_position", null);
    }

    @Test(expected = NavigationInputException.class)
    public void given_movement_instructions_exist_but_not_one_is_understood_then_complain_about_it() throws Exception{
        MarsRoverPilot distributor = new MarsRoverPilot();
        LandingPositionInputValidator specialValidator = new LandingPositionInputValidator(){
            public boolean validateInitialLandingPosition(String inputLandingPosition) throws InitialLandingPositionException, MissingInitialLandingPositionException {
                return true;
            }
        };
        NavigationInputValidator specialNaviValidator = new NavigationInputValidator(){
            public boolean validateNavigationalInput(String navigationInput) throws Exception{
                throw new NavigationInputException("i think i am being asked to jump or something, which i cant do...!");
            }
        };
        distributor.setLandingPositionValidator(specialValidator);
        distributor.setNavigationValidator(specialNaviValidator);
        distributor.distribute_candies("38W", "ZUSW");
    }


    @Test
    public void given_movement_instructions_exist_but_at_least_one_is_understood_then_proceed_normally() throws Exception{
        MarsRoverPilot distributor = new MarsRoverPilot();
        LandingPositionInputValidator specialValidator = new LandingPositionInputValidator(){
            public boolean validateInitialLandingPosition(String inputLandingPosition) throws InitialLandingPositionException, MissingInitialLandingPositionException {
                return true;
            }
        };
        NavigationInputValidator specialNaviValidator = new NavigationInputValidator(){
            public boolean validateNavigationalInput(String navigationInput) throws Exception{
                return true;
            }
        };
        distributor.setLandingPositionValidator(specialValidator);
        distributor.setNavigationValidator(specialNaviValidator);
       //some instruction make sense, others not
        Assert.assertNotNull(distributor.distribute_candies("66S", "LRZMFWI7"));
    }



}

package unit;

import mars.exceptions.MissingInitialLandingPositionException;
import mars.validators.LandingPositionInputValidator;
import mars.exceptions.InitialLandingPositionException;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 * Created by michaelnyika on 11/8/14.
 *
 * These tests make sure that we can construct a GridPosition Object that
 * represents the initial landing position of the Rover. If we cannot construct this domain
 * object, then everything else is seemingly futile
 */
public class LandingPositionInputValidatorTests {

    private LandingPositionInputValidator landingPositionValidator;

    @Before
    public void before_we_initiate_the_checks(){
        landingPositionValidator = new LandingPositionInputValidator();
    }

    @Test(expected=MissingInitialLandingPositionException.class)
    public void given_null_landing_position_then_complain_about_it() throws Exception{
        landingPositionValidator.validateInitialLandingPosition(null);
    }

    @Test(expected=InitialLandingPositionException.class)
    public void given_landing_position_with_no_characters_then_complain_about_it() throws Exception{
        landingPositionValidator.validateInitialLandingPosition("");
    }

    @Test(expected=InitialLandingPositionException.class)
    public void given_landing_position_with_two_characters_then_complain_about_it() throws Exception{
        landingPositionValidator.validateInitialLandingPosition("2E");
    }

    @Test(expected=InitialLandingPositionException.class)
    public void given_landing_position_with_one_character_then_complain_about_it() throws Exception{
        landingPositionValidator.validateInitialLandingPosition("6");
    }

    @Test(expected=InitialLandingPositionException.class)
    public void given_landing_position_with_four_character_then_complain_about_it() throws Exception{
        landingPositionValidator.validateInitialLandingPosition("63SP");
    }

    @Test(expected=InitialLandingPositionException.class)
    public void given_landing_position_with_nonalphanumeric_then_complain_about_it() throws Exception{
        landingPositionValidator.validateInitialLandingPosition("%");
    }

    @Test()
    public void given_landing_position_conforming_to_pos_regex_then_allow() throws Exception{
        Assert.assertTrue(landingPositionValidator.validateInitialLandingPosition("23E"));
    }

}

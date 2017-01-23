package unit;

import mars.constants.MovementInstruction;
import mars.exceptions.MissingInitialLandingPositionException;
import mars.exceptions.MissingNavigationInputException;
import mars.exceptions.NavigationInputException;
import mars.validators.LandingPositionInputValidator;
import mars.validators.NavigationInputValidator;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 * Created by michaelnyika on 11/8/14.
 *
 * These tests check that the instructions for movement and orientation
 * are present and reasonably executable
 */
public class NavigationInputValidatorTests {

    private NavigationInputValidator navigation_input_validator;

    @Before
    public void before_we_initiate_the_navigational_checks(){
        navigation_input_validator = new NavigationInputValidator();
    }

    @Test(expected=MissingNavigationInputException.class)
    public void given_null_navigational_input_then_complain() throws Exception{
        navigation_input_validator.validateNavigationalInput(null);
    }

    @Test(expected=NavigationInputException.class)
    public void given_empty_string_for_navigational_input_then_complain() throws Exception{
        navigation_input_validator.validateNavigationalInput("");
    }

    @Test
    public void given_input_string_really_from_NASA_for_navigational_input_then_approve() throws Exception{
        Assert.assertTrue(navigation_input_validator.validateNavigationalInput("LRRLMMLRRL"));
    }

    @Test
    public void given_input_string_with_single_character_for_navigational_input_then_approve() throws Exception{
        Assert.assertTrue(navigation_input_validator.validateNavigationalInput(MovementInstruction.ADVANCE.VALUE()));
    }

    @Test(expected=NavigationInputException.class)
    public void given_string_with_only_special_characters_for_navigational_input_then_complain() throws Exception{
        navigation_input_validator.validateNavigationalInput("&§$?$/=");
    }

    @Test(expected=NavigationInputException.class)
    public void given_string_with_only_numerals_for_navigational_input_then_complain() throws Exception{
        navigation_input_validator.validateNavigationalInput("13214594");
    }
}

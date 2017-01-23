package unit;

import mars.builders.OrientationMapBuilder;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 * Created by michaelnyika on 11/9/14.
 *
 * These tests simply make sure that nothing goes wrong building
 * the rule maps for orienting the rover
 */
public class OrientationMapBuilderTests {

   private OrientationMapBuilder testSubject;

    @Before
    public void start_it_up(){
        testSubject = new OrientationMapBuilder();
    }


    @Test
    public void construct_NORTH_bound_orientation_rules_without_problems() {
        try {
            testSubject.compose_North_Orientation_Rules();
        } catch (Exception somethingNotGood) {
            Assert.fail("Nothing was meant to go wrong while constructing the north-bound directional rules");
        }
    }

    @Test
    public void construct_SOUTH_bound_orientation_rules_without_problems() {
        try {
            testSubject.compose_South_Orientation_Rules();
        } catch (Exception somethingNotGood) {
            Assert.fail("Nothing was meant to go wrong while constructing the south-bound directional rules");
        }
    }

    @Test
    public void construct_EAST_bound_orientation_rules_without_problems() {
        try {
            testSubject.compose_East_Orientation_Rules();
        } catch (Exception somethingNotGood) {
            Assert.fail("Nothing was meant to go wrong while constructing the east-bound directional rules");
        }
    }

    @Test
    public void construct_WEST_bound_orientation_rules_without_problems() {
        try {
            testSubject.compose_West_Orientation_Rules();
        } catch (Exception somethingNotGood) {
            Assert.fail("Nothing was meant to go wrong while constructing the west-bound directional rules");
        }
    }

    @Test
    public void construct_Master_Orientation_Mapping_rules_without_problems() {
        try {
            testSubject.construct_Master_Orientation_Mapping();
        } catch (Exception somethingNotGood) {
            Assert.fail("Nothing was meant to go wrong while constructing Master-Orientation directional rules");
        }
    }
}

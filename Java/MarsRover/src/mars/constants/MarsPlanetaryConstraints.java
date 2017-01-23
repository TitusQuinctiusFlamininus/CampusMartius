package mars.constants;

/**
 * Created by michaelnyika on 11/8/14.
 *
 * This enum just gives an idea of how large the
 * movement area is and gives us a guide as to what the
 * maximum movement constraints of the rover can be. In this
 * case (Mars), we have a 9x9 grid. Our left-bottom grid
 * is labelled 0,0 and the top-right is labelled 9x9
 */
public enum MarsPlanetaryConstraints {

    //For planetary ground level specs: MARS
    MAX_GRID_INDEX(9),
    MIN_GRID_INDEX(0);

    private int VALUE;
    private MarsPlanetaryConstraints(int theIndexValue){
        this.VALUE = theIndexValue;
    }

    public int VALUE(){
        return this.VALUE;
    }

}

/**
 * Example 1
 *
 * 1) Opens plant and root parameters from a file
 * 2) Simulates root growth
 * 3) Outputs a VTP (for vizualisation in ParaView)
 *    In Paraview: use tubePLot.py script for fancy visualisation (Macro/Add new macro...), apply after opening file
 *
 *  Additionally, exports the line segments as .txt file to import into Matlab for postprocessing
 */
using namespace std;

void example_lbrai2219()
{
    RootSystem rootsystem;

    /*
     * Plant and root parameter from a file
     */
    rootsystem.openFile("param", "inputs/");
    //rootsystem.writeParameters(std::cout);

    /*
     * Set geometry
     */

    SDF_PlantBox box(900,900,900);
    rootsystem.setGeometry(&box);

    /*
     * Initialize
     */
    rootsystem.initialize();

    /*
     * Simulate
     */
	rootsystem.simulate();
	SegmentAnalyser analysis(rootsystem);
    analysis.write("outputs/current_rootsystem.txt");

	cout << "fin\n";

}

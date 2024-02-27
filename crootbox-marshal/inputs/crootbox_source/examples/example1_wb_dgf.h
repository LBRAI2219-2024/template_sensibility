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

void example1_wb_dgf()
{
    RootSystem rootsystem;

    string name = "param";

    /*
     * Plant and root parameter from a file
     */
    rootsystem.openFile(name, "www/");
    //rootsystem.writeParameters(std::cout);

    /*
     * Set geometry
     */
    //creates a box
    SDF_PlantBox box(900,900,900);
    rootsystem.setGeometry(&box);
    /*
     * Initialize
     */
    rootsystem.initialize();

    /*
     * Simulate
     */
     double simtime = 60; // 20, 40, 60 days
     double dt = 10; // try other values here
	 int t = 0;
     int N = round(simtime/dt);
    
     for (int i=0; i<N; i++) {
		 t = t+dt;
		 rootsystem.simulate(dt);
		 SegmentAnalyser analysis(rootsystem);
		 analysis.write(std::to_string(t)+"_rootsystem.txt");
     }

    /*
     * Export final result (as vtp)
     */
    //rootsystem.write(name+".vtp");

    /*
     * Export segments in RSML format
     */
    //rootsystem.write(name+".rsml");

    /*
     * Export dgf format
     */
    //SegmentAnalyser analysis(rootsystem);
    //analysis.write(name+".dgf");

    /*
      Total length and surface
     */
    //double l = analysis.getSummed(RootSystem::st_length);
    //std::cout << "Visible Length " << l << " cm \n";

	cout << "fin\n";
    //cout << "Finished with a total of " << rootsystem.getNumberOfNodes()<< " nodes\n";
    //cout << "Finished with a total of " << rootsystem.getNumberOfNodes()<< " nodes\n";

}

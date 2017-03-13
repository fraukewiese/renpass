# renpass
Renewable Energy Pathways Simulation System

DESCRIPTION
renpass (Renewable Energy Pathways Simulation System) is an open source simulation energy model (published under [GNU GPL 3](opensource.org/licenses/GPL-3.0)) which has the goal to fulfil the requirements of full transparency and the possibility to image 100% renewable energy systems as well as today's system on a high regional and time resolution.

STATUS, FURTHER DEVELOPEMENT AND CONNECTION OTHER MODELS
renpass was developed by Frauke Wiese and Gesine Bökenkamp and contributions by others. The status on GitHub is from 2015. The model itself has not been further developed since then, but an open energy modelling framework [(oemof)](https://github.com/oemof/oemof) and the application [renpassG!S](https://github.com/znes/renpass_gis) have evolved from the initial idea of renpass, while being written in Python and providing extended functionality.

AIM  
- Techno-Economic Simulation of 100 % renewable energy systems and pathways
- Comparability, transparency and reliability of assumptions for credibility
- Dependency on the active participation of the users
- Retrievability of mistakes in the calculation and data
- Making gaps in data availability obvious
- Modular Setting to combine expert knowledge of different areas

FUNCTIONALITY  
The installed capacities and expansion pathways of the different energy sources are set exogenously for the period to be analysed. For each time step, the production of wind, solar and run-of-river electricity is subtracted from the demand. The so-called residual load is then supplied by the least expensive combination of the fully controllable production plants, storage units and grid utilisation. The utilisation of controllable capacity in renpass is based on regional dispatch in each grid region followed by balancing between the regions within the limits of the grid capacity. More explanation can be found in the renpass manual.

SOFTWARE AND LICENSE  
All software is open source software and publicly available. renpass is published under [GNU General Public License 3](http://opensource.org/licenses/GPL-3.0)
The license guarantees you the freedoms to use, study, share (copy), and modify the software. It is a copyleft license, which means that you can distribute derived works only under the same license terms.

CONTRIBUTION  
Please report any bugs you can find. renpass is in a dynamic process of extensions and changes. If you want to help to improve this model by finding bugs and to broaden renpass by adding new code pieces, better data, more clever algorithms, or other improvements, you are very welcome!

NEED FOR TRANSPARENCY   
Fundamental changes of the energy system like a transition to a 100 % renewable energy supply need an extremely high acceptance of the general public. The necessity of new production, transmission and storage facilities can only be analysed by highly complex analytical models, which usually are proprietary. In general it is not transparent how the results are derived. For the advancement of science as well as for the development of public trust in the modelling results, it is necessary that energy models are not developed and used in parallel isolation or even deliberately closed off. On the contrary they need to be transparent and openly available.

OPEN DATA AND OPEN SOURCE IN ENERGY SYSTEM MODELING   
The role of open data and open source as an approach to meet challenges in energy modeling as well as the model renpass is further described in the doctoral thesis which you can find in the .zip-file.

INSTALLATION    
renpass is written in the programming language R. Pathway assumptions, data and results are stored in a MySQL-database.
You can find :
- manual for the installation of R and MySQL
- manual for renpass
- PhD thesis describing renpass and its open source character
- code of renpass
- databases of renpass (the )
in this repository

CONTACT   
The weather database was to big to provide it here, so please try if it is still availabe [here](https://www.uni-flensburg.de/fileadmin/content/zentren/znes/dokumente/projekte/renpass.zip) as part of the downloadable zip-file or, if that does not work, contact Frauke Wiese if you need it. Also in case of any questions contact Frauke via GitHub.

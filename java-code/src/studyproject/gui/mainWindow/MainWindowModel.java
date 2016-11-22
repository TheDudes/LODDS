package studyproject.gui.mainWindow;

import javax.annotation.PostConstruct;

import studyproject.API.Lvl.Mid.LODDS;

public class MainWindowModel {
	private LODDS lodds;

	@PostConstruct
	public void init(){
		lodds = new LODDS();
	}
	
	public LODDS getLodds() {
		return lodds;
	}

	public void setLodds(LODDS lodds) {
		this.lodds = lodds;
	}
	
}

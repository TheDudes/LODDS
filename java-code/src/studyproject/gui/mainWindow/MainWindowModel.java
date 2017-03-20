package studyproject.gui.mainWindow;

import javax.annotation.PostConstruct;

import studyproject.API.Lvl.Mid.Lodds.Lodds;
import studyproject.gui.introduction.IntroductionInterface;

public class MainWindowModel implements IntroductionInterface {

	private Lodds lodds;

	@PostConstruct
	public void init() {
		lodds = new Lodds();
	}

	public Lodds getLodds() {
		return lodds;
	}

	public void setLodds(Lodds lodds) {
		this.lodds = lodds;
	}

	@Override
	public String getViewDiscription() {
		return "The MainWindow shows all important parts of the program. "
				+ "These are divided into 5 zones: The MenuBar at the top, "
				+ "a list of user on the left-hand side, downloadable files "
				+ "in its centre, on going downloads at the right and a "
				+ "list of log-messages at the bottom of the MainWindow.";
	}

	@Override
	public String getViewTitle() {
		return "Main Window";
	}
}

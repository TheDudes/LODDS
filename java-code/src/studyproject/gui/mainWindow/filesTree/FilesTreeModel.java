package studyproject.gui.mainWindow.filesTree;

import studyproject.gui.introduction.IntroductionInterface;

public class FilesTreeModel implements IntroductionInterface {

	@Override
	public String getViewDiscription() {
		return "The FilesTree shows all folders and files which are being"
				+ " shared by the selected user. If you want to download "
				+ "some files, just select one or more of them and press "
				+ "the 'Download' button in the right-bottom corner. "
				+ "You want to download a whole folder? No Problem! "
				+ "Just select the folder and press the 'Download'-Button. "
				+ "You even can mix the selection with folders and files and "
				+ "download them all at once. After pressing the "
				+ "'Download'-Button, a dialog to choose a directory will "
				+ "appear. Press the 'OK' button if the place where to save "
				+ "the files is selected.";
	}

	@Override
	public String getViewTitle() {
		return "Files Tree";
	}

}

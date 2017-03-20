package studyproject.gui.macDockMenu;

import java.awt.MenuItem;
import java.awt.PopupMenu;

import javafx.application.Platform;
import studyproject.gui.mainWindow.topMenu.TopMenuPresenter;
import studyproject.gui.mainWindow.topMenu.TopMenuView;

/**
 * GUI for Mac OS specific dock menu
 * 
 * @author gitmalong
 *
 */
public class MacDockMenuPresenter {

	private TopMenuPresenter topMenuPresenter;

	/**
	 * Creates dock menu entries
	 */
	public void createMenus() {
		topMenuPresenter = (TopMenuPresenter) (new TopMenuView()).getPresenter();
		PopupMenu menu = new PopupMenu();
		
		// Share folder
		MenuItem shareFolder = new MenuItem("Share folder");
		// Need to be in fx application thread
		shareFolder.addActionListener(e -> Platform.runLater(new Runnable() {
			@Override
			public void run() {
				topMenuPresenter.shareFolderPressed();
			}
		}));
		menu.add(shareFolder);
		
		// Unshare folder
		MenuItem unshareFolder = new MenuItem("Unshare folder");
		// Need to be in fx application thread
		unshareFolder.addActionListener(e -> Platform.runLater(new Runnable() {
			@Override
			public void run() {
				topMenuPresenter.unshareFolderPressed();
			}
		}));
		menu.add(unshareFolder);
		
		// set the dock menu
		com.apple.eawt.Application app = com.apple.eawt.Application.getApplication();
		app.setDockMenu(menu);
	}

}

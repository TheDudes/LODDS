package studyproject.gui.macDockMenu;

import java.awt.MenuItem;
import java.awt.PopupMenu;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javafx.application.Platform;
import studyproject.gui.Core.Utils;

/**
 * GUI for Mac OS specific dock menu
 * 
 * @author gitmalong
 *
 */
public class MacDockMenuPresenter {

	/**
	 * Creates dock menu entries
	 */
	public void createMenus() {
		MenuItem shareFolder = new MenuItem("Share folder");
		shareFolder.addActionListener(new ActionListener() {

			@Override
			public void actionPerformed(ActionEvent e) {
				// Share folder
				Platform.runLater(new Runnable() {

					@Override
					public void run() {
						Utils.shareFolderPressed();
					}
				});

			}
		});

		PopupMenu menu = new PopupMenu();
		menu.add(shareFolder);

		// set the dock menu
		com.apple.eawt.Application app = com.apple.eawt.Application.getApplication();
		app.setDockMenu(menu);
	}

}

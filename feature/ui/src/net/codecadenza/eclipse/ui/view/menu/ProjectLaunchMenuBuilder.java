/*
 * This file is part of CodeCadenza, a set of tools, libraries and plug-ins
 * for modeling and creating Java-based enterprise applications.
 * For more information visit:
 *
 * https://github.com/codecadenza/
 *
 * This software is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
 */
package net.codecadenza.eclipse.ui.view.menu;

import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.service.launch.LaunchService;
import net.codecadenza.eclipse.ui.view.ProjectView;
import org.eclipse.debug.ui.DebugUITools;
import org.eclipse.debug.ui.IDebugUIConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Tree;

/**
 * <p>
 * Helper class for creating a context menu to either run, debug or build the applications of the currently selected project
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ProjectLaunchMenuBuilder extends AbstractMenuBuilder<Project> {
	private enum Mode {
		RUN, DEBUG, BUILD
	}

	private final Menu parentMenu;

	/**
	 * Constructor
	 * @param parentMenu
	 * @param view
	 * @param tree
	 */
	public ProjectLaunchMenuBuilder(Menu parentMenu, ProjectView view, Tree tree) {
		super(view, tree);

		this.parentMenu = parentMenu;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.view.menu.AbstractMenuBuilder#createMenu()
	 */
	@Override
	public Menu createMenu() {
		final var itemRun = new MenuItem(parentMenu, SWT.CASCADE);
		itemRun.setText(TITLE_RUN);

		final var menuRun = new Menu(itemRun);
		itemRun.setMenu(menuRun);

		// Add a menu item to run the applications of the currently selected project
		final var itemRunApps = new MenuItem(menuRun, SWT.NONE);
		itemRunApps.setText(TITLE_RUN);
		itemRunApps.setImage(DebugUITools.getImage(IDebugUIConstants.IMG_ACT_RUN));

		itemRunApps.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				launchApplications(Mode.RUN, TITLE_RUN);
			}
		});

		// Add a menu item to debug the applications of the currently selected project
		final var itemDebugApps = new MenuItem(menuRun, SWT.NONE);
		itemDebugApps.setText(TITLE_DEBUG);
		itemDebugApps.setImage(DebugUITools.getImage(IDebugUIConstants.IMG_ACT_DEBUG));

		itemDebugApps.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				launchApplications(Mode.DEBUG, TITLE_DEBUG);
			}
		});

		// Add a menu item to build the currently selected project
		final var itemBuildApps = new MenuItem(menuRun, SWT.NONE);
		itemBuildApps.setText(TITLE_BUILD);
		itemBuildApps.setImage(DebugUITools.getImage(IDebugUIConstants.IMG_OBJS_REGISTER));

		itemBuildApps.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				launchApplications(Mode.BUILD, TITLE_BUILD);
			}
		});

		return menuRun;
	}

	/**
	 * Run, debug or build the applications of the selected project
	 * @param mode
	 * @param title
	 */
	private void launchApplications(Mode mode, String title) {
		try {
			if (mode == Mode.RUN)
				new LaunchService(getSelectedObject()).runApplications();
			else if (mode == Mode.DEBUG)
				new LaunchService(getSelectedObject()).debugApplications();
			else
				new LaunchService(getSelectedObject()).buildProject();
		}
		catch (final Exception ex) {
			final var message = "Error while launching! Message: " + ex.getMessage();

			MessageDialog.openWarning(shell, title, message);
		}
	}

}

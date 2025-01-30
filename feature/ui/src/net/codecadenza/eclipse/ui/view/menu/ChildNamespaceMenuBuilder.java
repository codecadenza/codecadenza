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

import static net.codecadenza.eclipse.shared.Constants.IMG_PERFORM_SYNC;
import static net.codecadenza.eclipse.shared.Constants.IMG_REFRESH;

import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.resource.CodeCadenzaResourcePlugin;
import net.codecadenza.eclipse.service.build.ProjectBuildFactory;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.eclipse.ui.view.ProjectView;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Tree;

/**
 * <p>
 * Helper class to create a context menu for tree view items that represent sub-namespaces
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the specific {@link Namespace} type that should be used for this menu builder
 */
public class ChildNamespaceMenuBuilder<T extends Namespace> extends AbstractMenuBuilder<T> {
	/**
	 * Constructor
	 * @param view
	 * @param tree
	 */
	public ChildNamespaceMenuBuilder(ProjectView view, Tree tree) {
		super(view, tree);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.view.menu.AbstractMenuBuilder#createMenu()
	 */
	@Override
	public Menu createMenu() {
		final var menu = new Menu(tree);

		// Add a menu item to refresh the selected namespace
		final var itemRefresh = new MenuItem(menu, SWT.NONE);
		itemRefresh.setText(TITLE_REFRESH);
		itemRefresh.setImage(CodeCadenzaResourcePlugin.getImage(IMG_REFRESH));

		itemRefresh.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				view.addNamespaceItems(getSelectedItem());
			}
		});

		// Add a menu item to rebuild the source files of all objects of the selected namespace
		final var itemRebuild = new MenuItem(menu, SWT.NONE);
		itemRebuild.setText(TITLE_REBUILD);
		itemRebuild.setImage(CodeCadenzaResourcePlugin.getImage(IMG_PERFORM_SYNC));

		itemRebuild.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final Namespace n = getSelectedObject();

				final var job = new Job("Building objects of namespace " + n.getName()) {
					/*
					 * (non-Javadoc)
					 * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)
					 */
					@Override
					protected IStatus run(IProgressMonitor monitor) {
						try {
							ProjectBuildFactory.getBuildService(n.getProject()).buildObjectsOfNamespace(n, true, monitor);
						}
						catch (final Exception e) {
							shell.getDisplay().syncExec(() -> CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(e));
						}

						monitor.done();

						return Status.OK_STATUS;
					}
				};

				job.schedule();
			}
		});

		return menu;
	}

}

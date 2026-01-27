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

import net.codecadenza.eclipse.model.domain.DomainNamespace;
import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.service.build.ProjectBuildFactory;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.tools.reverse.editor.ReverseEditor;
import net.codecadenza.eclipse.tools.reverse.editor.ReverseEditorInput;
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
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;

/**
 * <p>
 * Helper class to create a context menu for tree view items that represent domain object namespaces
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DomainNamespaceMenuBuilder extends ChildNamespaceMenuBuilder<DomainNamespace> {
	/**
	 * Constructor
	 * @param view
	 * @param tree
	 */
	public DomainNamespaceMenuBuilder(ProjectView view, Tree tree) {
		super(view, tree);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.ui.view.menu.ChildNamespaceMenuBuilder#createMenu()
	 */
	@Override
	public Menu createMenu() {
		final Menu menu = super.createMenu();

		// Add a menu item to open the diagram editor for the selected namespace
		final var itemOpenDiagram = new MenuItem(menu, SWT.NONE);
		itemOpenDiagram.setText(TITLE_OPEN);

		itemOpenDiagram.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final DomainNamespace namespace = getSelectedObject();

				try {
					final IEditorPart editor = EclipseIDEService.openInEditor(namespace.getDiagramFile());

					if (editor != null)
						editor.addPropertyListener((_, _) -> view.refreshTree());
				}
				catch (final Exception ex) {
					CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
				}
			}
		});

		// Add a menu item to open the reverse engineering editor
		final var itemReverse = new MenuItem(menu, SWT.NONE);
		itemReverse.setText(TITLE_REV_ENG);

		itemReverse.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (EclipseIDEService.openDiagramExists(view))
					return;

				final Namespace namespace = getSelectedObject();
				final Project project = namespace.getProject();
				final var input = new ReverseEditorInput(namespace);
				final IWorkbenchWindow workbenchWindow = view.getViewSite().getWorkbenchWindow();

				try {
					final var editor = (ReverseEditor) workbenchWindow.getActivePage().openEditor(input, ReverseEditor.ID);

					editor.addPropertyListener((_, propId) -> {
						if (propId == ReverseEditor.STATE_SUCCESS)
							buildAllDomainObjects(project);

						view.refreshTree();
					});
				}
				catch (final PartInitException ex) {
					CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
				}
			}
		});

		return menu;
	}

	/**
	 * @param project
	 */
	private void buildAllDomainObjects(final Project project) {
		final var job = new Job("Building domain objects") {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)
			 */
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				try {
					for (final Namespace namespace : project.getDomainNamespace().getChildNamespaces())
						ProjectBuildFactory.getBuildService(project).buildObjectsOfNamespace(namespace, true, monitor);
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

}

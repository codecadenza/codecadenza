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
package net.codecadenza.eclipse.ui.wizard;

import java.lang.reflect.InvocationTargetException;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.eclipse.ui.operation.DiagramFileCreationOperation;
import net.codecadenza.eclipse.ui.operation.ProjectCreationOperation;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.actions.WorkspaceModifyDelegatingOperation;
import org.eclipse.ui.wizards.newresource.BasicNewProjectResourceWizard;

/**
 * <p>
 * Wizard for creating new projects
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CreateNewProjectWizard extends Wizard implements INewWizard {
	public static final String ID = "net.codecadenza.eclipse.ui.wizard.CreateNewProjectWizard";

	private ProjectWizardPage pageProject;
	private DBConnectionWizardPage pageDBConnection;
	private ApplicationWizardPage pageApp;
	private IntegrationWizardPage pageIntegration;
	private BuildConfigWizardPage pageBuild;
	private TestingWizardPage pageTesting;

	/**
	 * Constructor
	 */
	public CreateNewProjectWizard() {
		setWindowTitle("Create new CodeCadenza project");
		setNeedsProgressMonitor(false);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.wizard.Wizard#addPages()
	 */
	@Override
	public void addPages() {
		super.addPages();

		int pageCount = 0;

		pageProject = new ProjectWizardPage(pageCount++);
		pageDBConnection = new DBConnectionWizardPage(pageCount++);
		pageApp = new ApplicationWizardPage(pageCount++);
		pageIntegration = new IntegrationWizardPage(pageCount++, pageApp, pageProject);
		pageTesting = new TestingWizardPage(pageCount++, pageApp, pageIntegration);
		pageBuild = new BuildConfigWizardPage(pageCount, pageProject, pageApp, pageIntegration, pageTesting);

		addPage(pageProject);
		addPage(pageDBConnection);
		addPage(pageApp);
		addPage(pageIntegration);
		addPage(pageTesting);
		addPage(pageBuild);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.wizard.Wizard#performFinish()
	 */
	@Override
	public boolean performFinish() {
		if (pageProject != null) {
			this.getShell().setVisible(false);

			final var runnable = new ProjectCreationOperation(pageProject, pageDBConnection, pageApp, pageBuild, pageIntegration,
					pageTesting);
			final var op = new WorkspaceModifyDelegatingOperation(runnable);

			try {
				getContainer().run(false, true, op);
			}
			catch (final InvocationTargetException e) {
				CodeCadenzaUserInterfacePlugin.getInstance().logError(e);
				return false;
			}
			catch (final InterruptedException _) {
				Thread.currentThread().interrupt();

				CodeCadenzaUserInterfacePlugin.getInstance().logInfo("Operation has been interrupted!");
				return false;
			}

			BasicNewProjectResourceWizard.updatePerspective(null);

			// Finally we create the domain diagram files. Note that this must be done at this location as it is very hard or even
			// impossible to launch a second wizard within the project creation operation!
			if (pageProject.getProject() != null)
				pageProject.getModelURISet()
						.forEach(uri -> DiagramFileCreationOperation.initializeAndOpenDomainDiagram(pageProject.getProject(), uri));
		}

		return true;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.wizard.Wizard#canFinish()
	 */
	@Override
	public boolean canFinish() {
		// Force the user to go through all pages until the wizard can be finished!
		return getContainer().getCurrentPage() == pageBuild;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench, org.eclipse.jface.viewers.IStructuredSelection)
	 */
	@Override
	public void init(IWorkbench workbench, IStructuredSelection selection) {
		// No implementation required!
	}

}

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
package net.codecadenza.eclipse.ui.operation;

import static net.codecadenza.eclipse.shared.Constants.MODEL_FOLDER;

import java.lang.reflect.InvocationTargetException;
import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaNewDiagramFileWizard;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.transaction.TransactionalEditingDomain;
import org.eclipse.gmf.runtime.emf.core.GMFEditingDomainFactory;
import org.eclipse.jface.dialogs.ProgressMonitorDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.wizard.IWizardContainer;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

/**
 * <p>
 * Utility class to create a domain diagram file
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DiagramFileCreationOperation {
	/**
	 * Prevent instantiation
	 */
	private DiagramFileCreationOperation() {

	}

	/**
	 * @param project
	 * @param processModelURI
	 */
	public static void initializeAndOpenDomainDiagram(Project project, URI processModelURI) {
		final String targetProjectName = project.getTargetProjectName(BuildArtifactType.DOMAIN);
		final URI resourceURI = URI
				.createPlatformResourceURI("/" + targetProjectName + "/" + MODEL_FOLDER + "/" + processModelURI.lastSegment(), true);
		final TransactionalEditingDomain editingDomain = GMFEditingDomainFactory.INSTANCE.createEditingDomain();
		final ResourceSet resourceSet = editingDomain.getResourceSet();
		EObject diagramRoot = null;

		try {
			final Resource resource = resourceSet.getResource(resourceURI, true);
			diagramRoot = resource.getContents().get(0);
		}
		catch (final Exception ex) {
			CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
			return;
		}

		// The wizard will create our diagram. We don't want to open the wizard, but use it to create the diagram, bring it in a state
		// where this is possible.
		final var wizard = new CodeCadenzaNewDiagramFileWizard(resourceURI, diagramRoot, editingDomain) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaNewDiagramFileWizard#performFinish()
			 */
			@Override
			public boolean performFinish() {
				try {
					return super.performFinish();
				}
				finally {
					fileCreationPage.dispose();
				}
			}
		};

		// Set a dummy container, to just run the action
		wizard.setContainer(new IWizardContainer() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.jface.operation.IRunnableContext#run(boolean, boolean,
			 * org.eclipse.jface.operation.IRunnableWithProgress)
			 */
			@Override
			public void run(boolean fork, boolean cancelable, IRunnableWithProgress runnable)
					throws InvocationTargetException, InterruptedException {
				final var dialog = new ProgressMonitorDialog(Display.getCurrent().getActiveShell());
				dialog.run(fork, cancelable, runnable);
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.jface.wizard.IWizardContainer#updateWindowTitle()
			 */
			@Override
			public void updateWindowTitle() {
				// No implementation required!
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.jface.wizard.IWizardContainer#updateTitleBar()
			 */
			@Override
			public void updateTitleBar() {
				// No implementation required!
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.jface.wizard.IWizardContainer#updateMessage()
			 */
			@Override
			public void updateMessage() {
				// No implementation required!
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.jface.wizard.IWizardContainer#updateButtons()
			 */
			@Override
			public void updateButtons() {
				// No implementation required!
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.jface.wizard.IWizardContainer#showPage(org.eclipse.jface.wizard.IWizardPage)
			 */
			@Override
			public void showPage(IWizardPage page) {
				// No implementation required!
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.jface.wizard.IWizardContainer#getShell()
			 */
			@Override
			public Shell getShell() {
				return null;
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.jface.wizard.IWizardContainer#getCurrentPage()
			 */
			@Override
			public IWizardPage getCurrentPage() {
				return null;
			}
		});

		// Perform the diagram creation
		wizard.performFinish();
	}

}

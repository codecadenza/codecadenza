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
package net.codecadenza.eclipse.diagram.domain.part;

import static net.codecadenza.eclipse.shared.Constants.DIAGRAM_FILE_EXTENSION;

import java.io.IOException;
import java.util.LinkedList;
import net.codecadenza.eclipse.diagram.domain.edit.parts.DomainNamespaceEditPart;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.operations.OperationHistoryFactory;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.transaction.TransactionalEditingDomain;
import org.eclipse.gmf.runtime.common.core.command.CommandResult;
import org.eclipse.gmf.runtime.diagram.core.services.ViewService;
import org.eclipse.gmf.runtime.diagram.core.services.view.CreateDiagramViewOperation;
import org.eclipse.gmf.runtime.emf.commands.core.command.AbstractTransactionalCommand;
import org.eclipse.gmf.runtime.emf.core.util.EObjectAdapter;
import org.eclipse.gmf.runtime.notation.Diagram;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.osgi.util.NLS;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.dialogs.WizardNewFileCreationPage;

/**
 * <p>
 * Wizard for creating new domain diagram files
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CodeCadenzaNewDiagramFileWizard extends Wizard {
	protected WizardNewFileCreationPage fileCreationPage;
	private final ModelElementSelectionPage diagramRootElementSelectionPage;
	private final TransactionalEditingDomain myEditingDomain;

	/**
	 * Constructor
	 * @param domainModelURI
	 * @param diagramRoot
	 * @param editingDomain
	 */
	public CodeCadenzaNewDiagramFileWizard(URI domainModelURI, EObject diagramRoot, TransactionalEditingDomain editingDomain) {
		fileCreationPage = new WizardNewFileCreationPage(Messages.CodeCadenzaNewDiagramFileWizard_CreationPageName,
				StructuredSelection.EMPTY);
		fileCreationPage.setTitle(Messages.CodeCadenzaNewDiagramFileWizard_CreationPageTitle);
		fileCreationPage.createControl(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell());
		fileCreationPage.setWizard(this);
		fileCreationPage.setDescription(
				NLS.bind(Messages.CodeCadenzaNewDiagramFileWizard_CreationPageDescription, DomainNamespaceEditPart.MODEL_ID));

		IPath filePath;
		final String fileName = domainModelURI.trimFileExtension().lastSegment();

		if (domainModelURI.isPlatformResource())
			filePath = new Path(domainModelURI.trimSegments(1).toPlatformString(true));
		else if (domainModelURI.isFile())
			filePath = new Path(domainModelURI.trimSegments(1).toFileString());
		else
			throw new IllegalArgumentException("Unsupported URI: " + domainModelURI);

		fileCreationPage.setContainerFullPath(filePath);
		fileCreationPage.setFileName(CodeCadenzaDiagramEditorUtil.getUniqueFileName(filePath, fileName, DIAGRAM_FILE_EXTENSION));

		diagramRootElementSelectionPage = new DiagramRootElementSelectionPage(
				Messages.CodeCadenzaNewDiagramFileWizard_RootSelectionPageName);
		diagramRootElementSelectionPage.setTitle(Messages.CodeCadenzaNewDiagramFileWizard_RootSelectionPageTitle);
		diagramRootElementSelectionPage.setDescription(Messages.CodeCadenzaNewDiagramFileWizard_RootSelectionPageDescription);
		diagramRootElementSelectionPage.setModelElement(diagramRoot);

		myEditingDomain = editingDomain;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.wizard.Wizard#addPages()
	 */
	@Override
	public void addPages() {
		addPage(fileCreationPage);
		addPage(diagramRootElementSelectionPage);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.wizard.Wizard#performFinish()
	 */
	@Override
	public boolean performFinish() {
		final var affectedFiles = new LinkedList<IFile>();
		final IFile diagramFile = fileCreationPage.createNewFile();
		CodeCadenzaDiagramEditorUtil.setCharset(diagramFile);

		affectedFiles.add(diagramFile);

		final URI diagramModelURI = URI.createPlatformResourceURI(diagramFile.getFullPath().toString(), true);
		final ResourceSet resourceSet = myEditingDomain.getResourceSet();
		final Resource diagramResource = resourceSet.createResource(diagramModelURI);

		final var command = new AbstractTransactionalCommand(myEditingDomain,
				Messages.CodeCadenzaNewDiagramFileWizard_InitDiagramCommand, affectedFiles) {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.gmf.runtime.emf.commands.core.command.AbstractTransactionalCommand#
			 * doExecuteWithResult(org.eclipse.core.runtime.IProgressMonitor, org.eclipse.core.runtime.IAdaptable)
			 */
			@Override
			protected CommandResult doExecuteWithResult(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
				final int diagramVID = CodeCadenzaVisualIDRegistry.getDiagramVisualID(diagramRootElementSelectionPage.getModelElement());

				if (diagramVID != DomainNamespaceEditPart.VISUAL_ID)
					return CommandResult.newErrorCommandResult(Messages.CodeCadenzaNewDiagramFileWizard_IncorrectRootError);

				final Diagram diagram = ViewService.createDiagram(diagramRootElementSelectionPage.getModelElement(),
						DomainNamespaceEditPart.MODEL_ID, CodeCadenzaDiagramEditorPlugin.DIAGRAM_PREFERENCES_HINT);
				diagramResource.getContents().add(diagram);

				return CommandResult.newOKCommandResult();
			}
		};

		try {
			OperationHistoryFactory.getOperationHistory().execute(command, new NullProgressMonitor(), null);
			diagramResource.save(CodeCadenzaDiagramEditorUtil.getSaveOptions());
			CodeCadenzaDiagramEditorUtil.openDiagram(diagramResource);
		}
		catch (final ExecutionException e) {
			CodeCadenzaDiagramEditorPlugin.getInstance().logError("Unable to create model and diagram", e);
		}
		catch (final IOException ex) {
			CodeCadenzaDiagramEditorPlugin.getInstance().logError("Save operation failed for: " + diagramModelURI, ex);
		}
		catch (final PartInitException ex) {
			CodeCadenzaDiagramEditorPlugin.getInstance().logError("Unable to open editor", ex);
		}

		return true;
	}

	private static class DiagramRootElementSelectionPage extends ModelElementSelectionPage {
		/**
		 * @param pageName
		 */
		protected DiagramRootElementSelectionPage(String pageName) {
			super(pageName);
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.diagram.domain.part.ModelElementSelectionPage#getSelectionTitle()
		 */
		@Override
		protected String getSelectionTitle() {
			return Messages.CodeCadenzaNewDiagramFileWizard_RootSelectionPageSelectionTitle;
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.diagram.domain.part.ModelElementSelectionPage#validatePage()
		 */
		@Override
		protected boolean validatePage() {
			if (selectedModelElement == null) {
				setErrorMessage(Messages.CodeCadenzaNewDiagramFileWizard_RootSelectionPageNoSelectionMessage);
				return false;
			}

			final boolean result = ViewService.getInstance()
					.provides(new CreateDiagramViewOperation(new EObjectAdapter(selectedModelElement), DomainNamespaceEditPart.MODEL_ID,
							CodeCadenzaDiagramEditorPlugin.DIAGRAM_PREFERENCES_HINT));

			setErrorMessage(result ? null : Messages.CodeCadenzaNewDiagramFileWizard_RootSelectionPageInvalidSelectionMessage);

			return result;
		}
	}

}

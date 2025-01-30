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

import net.codecadenza.eclipse.diagram.domain.edit.parts.DomainNamespaceEditPart;
import org.eclipse.core.resources.IFile;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.common.util.WrappedException;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.transaction.TransactionalEditingDomain;
import org.eclipse.gmf.runtime.emf.core.GMFEditingDomainFactory;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;

/**
 * <p>
 * Diagram file action
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CodeCadenzaInitDiagramFileAction implements IObjectActionDelegate {
	private IWorkbenchPart targetPart;
	private URI domainModelURI;

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.IObjectActionDelegate#setActivePart(org.eclipse.jface.action.IAction, org.eclipse.ui.IWorkbenchPart)
	 */
	@Override
	public void setActivePart(IAction action, IWorkbenchPart targetPart) {
		this.targetPart = targetPart;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction, org.eclipse.jface.viewers.ISelection)
	 */
	@Override
	public void selectionChanged(IAction action, ISelection selection) {
		domainModelURI = null;
		action.setEnabled(false);

		if (!(selection instanceof final IStructuredSelection structuredSelection) || selection.isEmpty())
			return;

		final var file = (IFile) structuredSelection.getFirstElement();
		domainModelURI = URI.createPlatformResourceURI(file.getFullPath().toString(), true);
		action.setEnabled(true);
	}

	/**
	 * @return the shell
	 */
	private Shell getShell() {
		return targetPart.getSite().getShell();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
	 */
	@Override
	public void run(IAction action) {
		final TransactionalEditingDomain editingDomain = GMFEditingDomainFactory.INSTANCE.createEditingDomain();
		final ResourceSet resourceSet = editingDomain.getResourceSet();
		EObject diagramRoot = null;

		try {
			final Resource resource = resourceSet.getResource(domainModelURI, true);
			diagramRoot = resource.getContents().get(0);
		}
		catch (final WrappedException ex) {
			CodeCadenzaDiagramEditorPlugin.getInstance().logError("Unable to load resource: " + domainModelURI, ex);
		}

		if (diagramRoot == null) {
			MessageDialog.openError(getShell(), Messages.CodeCadenzaInitDiagramFileAction_InitDiagramFileResourceErrorDialogTitle,
					Messages.CodeCadenzaInitDiagramFileAction_InitDiagramFileResourceErrorDialogMessage);
			return;
		}

		final var wizard = new CodeCadenzaNewDiagramFileWizard(domainModelURI, diagramRoot, editingDomain);
		wizard.setWindowTitle(
				NLS.bind(Messages.CodeCadenzaInitDiagramFileAction_InitDiagramFileWizardTitle, DomainNamespaceEditPart.MODEL_ID));

		CodeCadenzaDiagramEditorUtil.runWizard(getShell(), wizard, "InitDiagramFile");
	}

}

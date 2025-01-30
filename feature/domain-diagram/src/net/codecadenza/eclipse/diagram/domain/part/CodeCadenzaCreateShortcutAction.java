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

import net.codecadenza.eclipse.diagram.domain.edit.commands.CodeCadenzaCreateShortcutDecorationsCommand;
import net.codecadenza.eclipse.diagram.domain.edit.parts.DomainNamespaceEditPart;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.operations.OperationHistoryFactory;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.common.util.WrappedException;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.gmf.runtime.common.core.command.ICommand;
import org.eclipse.gmf.runtime.diagram.ui.commands.CreateCommand;
import org.eclipse.gmf.runtime.diagram.ui.requests.CreateViewRequest;
import org.eclipse.gmf.runtime.emf.core.util.EObjectAdapter;
import org.eclipse.gmf.runtime.notation.Node;
import org.eclipse.gmf.runtime.notation.View;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;

/**
 * <p>
 * Action to create a shortcut
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CodeCadenzaCreateShortcutAction implements IObjectActionDelegate {
	private DomainNamespaceEditPart mySelectedElement;
	private Shell myShell;

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.IObjectActionDelegate#setActivePart(org.eclipse.jface.action.IAction, org.eclipse.ui.IWorkbenchPart)
	 */
	@Override
	public void setActivePart(IAction action, IWorkbenchPart targetPart) {
		myShell = targetPart.getSite().getShell();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction, org.eclipse.jface.viewers.ISelection)
	 */
	@Override
	public void selectionChanged(IAction action, ISelection selection) {
		mySelectedElement = null;

		if (selection instanceof final IStructuredSelection structuredSelection && structuredSelection.size() == 1
				&& structuredSelection.getFirstElement() instanceof final DomainNamespaceEditPart domainNamespaceEditPart)
			mySelectedElement = domainNamespaceEditPart;

		action.setEnabled(isEnabled());
	}

	/**
	 * @return true if the selected element is not null
	 */
	private boolean isEnabled() {
		return mySelectedElement != null;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
	 */
	@Override
	public void run(IAction action) {
		final var view = (View) mySelectedElement.getModel();
		final var elementChooser = new CodeCadenzaElementChooserDialog(myShell, view);
		final int result = elementChooser.open();

		if (result != Window.OK)
			return;

		final URI selectedModelElementURI = elementChooser.getSelectedModelElementURI();
		final EObject selectedElement;

		try {
			selectedElement = mySelectedElement.getEditingDomain().getResourceSet().getEObject(selectedModelElementURI, true);
		}
		catch (final WrappedException e) {
			CodeCadenzaDiagramEditorPlugin.getInstance().logError("Exception while loading object: " + selectedModelElementURI.toString(),
					e);
			return;
		}

		if (selectedElement == null)
			return;

		final var viewDescriptor = new CreateViewRequest.ViewDescriptor(new EObjectAdapter(selectedElement), Node.class, null,
				CodeCadenzaDiagramEditorPlugin.DIAGRAM_PREFERENCES_HINT);

		ICommand command = new CreateCommand(mySelectedElement.getEditingDomain(), viewDescriptor, view);
		command = command
				.compose(new CodeCadenzaCreateShortcutDecorationsCommand(mySelectedElement.getEditingDomain(), view, viewDescriptor));

		try {
			OperationHistoryFactory.getOperationHistory().execute(command, new NullProgressMonitor(), null);
		}
		catch (final ExecutionException e) {
			CodeCadenzaDiagramEditorPlugin.getInstance().logError("Unable to create shortcut", e);
		}
	}

}

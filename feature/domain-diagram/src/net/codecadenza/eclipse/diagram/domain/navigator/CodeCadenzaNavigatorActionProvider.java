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
package net.codecadenza.eclipse.diagram.domain.navigator;

import net.codecadenza.eclipse.diagram.domain.edit.parts.DomainNamespaceEditPart;
import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaDiagramEditor;
import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaDiagramEditorPlugin;
import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaVisualIDRegistry;
import net.codecadenza.eclipse.diagram.domain.part.Messages;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.emf.common.ui.URIEditorInput;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.emf.workspace.util.WorkspaceSynchronizer;
import org.eclipse.gmf.runtime.notation.Diagram;
import org.eclipse.gmf.runtime.notation.View;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.navigator.CommonActionProvider;
import org.eclipse.ui.navigator.ICommonActionConstants;
import org.eclipse.ui.navigator.ICommonActionExtensionSite;
import org.eclipse.ui.navigator.ICommonViewerWorkbenchSite;
import org.eclipse.ui.part.FileEditorInput;

/**
 * <p>
 * Navigator action provider
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CodeCadenzaNavigatorActionProvider extends CommonActionProvider {
	private boolean myContribute;
	private OpenDiagramAction myOpenDiagramAction;

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.navigator.CommonActionProvider#init(org.eclipse.ui.navigator.ICommonActionExtensionSite)
	 */
	@Override
	public void init(ICommonActionExtensionSite aSite) {
		super.init(aSite);

		if (aSite.getViewSite() instanceof final ICommonViewerWorkbenchSite commonViewerWorkbenchSite) {
			myContribute = true;
			makeActions(commonViewerWorkbenchSite);
		}
		else
			myContribute = false;
	}

	/**
	 * @param viewerSite
	 */
	private void makeActions(ICommonViewerWorkbenchSite viewerSite) {
		myOpenDiagramAction = new OpenDiagramAction(viewerSite);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.actions.ActionGroup#fillActionBars(org.eclipse.ui.IActionBars)
	 */
	@Override
	public void fillActionBars(IActionBars actionBars) {
		if (!myContribute)
			return;

		final var selection = (IStructuredSelection) getContext().getSelection();
		myOpenDiagramAction.selectionChanged(selection);

		if (myOpenDiagramAction.isEnabled())
			actionBars.setGlobalActionHandler(ICommonActionConstants.OPEN, myOpenDiagramAction);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.actions.ActionGroup#fillContextMenu(org.eclipse.jface.action.IMenuManager)
	 */
	@Override
	public void fillContextMenu(IMenuManager menu) {
		// No implementation required!
	}

	private class OpenDiagramAction extends Action {
		private Diagram myDiagram;
		private final ICommonViewerWorkbenchSite myViewerSite;

		/**
		 * @param viewerSite
		 */
		public OpenDiagramAction(ICommonViewerWorkbenchSite viewerSite) {
			super(Messages.NavigatorActionProvider_OpenDiagramActionName);

			myViewerSite = viewerSite;
		}

		/**
		 * @param selection
		 */
		public final void selectionChanged(IStructuredSelection selection) {
			myDiagram = null;

			if (selection.size() == 1) {
				Object selectedElement = selection.getFirstElement();

				if (selectedElement instanceof final CodeCadenzaNavigatorItem navigatorItem)
					selectedElement = navigatorItem.getView();
				else if (selectedElement instanceof final IAdaptable adaptable)
					selectedElement = adaptable.getAdapter(View.class);

				if (selectedElement instanceof final Diagram diagram
						&& DomainNamespaceEditPart.MODEL_ID.equals(CodeCadenzaVisualIDRegistry.getModelID(diagram)))
					myDiagram = diagram;
			}

			setEnabled(myDiagram != null);
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.action.Action#run()
		 */
		@Override
		public void run() {
			if (myDiagram == null || myDiagram.eResource() == null)
				return;

			final IEditorInput editorInput = getEditorInput();
			final IWorkbenchPage page = myViewerSite.getPage();

			try {
				page.openEditor(editorInput, CodeCadenzaDiagramEditor.ID);
			}
			catch (final PartInitException e) {
				CodeCadenzaDiagramEditorPlugin.getInstance().logError("Exception while openning diagram", e);
			}
		}

		/**
		 * @return the editor input
		 */
		private IEditorInput getEditorInput() {
			for (final EObject nextEObject : myDiagram.eResource().getContents()) {
				if (nextEObject == myDiagram)
					return new FileEditorInput(WorkspaceSynchronizer.getFile(myDiagram.eResource()));

				if (nextEObject instanceof Diagram)
					break;
			}

			final URI uri = EcoreUtil.getURI(myDiagram);
			final var editorName = uri.lastSegment() + "#" + myDiagram.eResource().getContents().indexOf(myDiagram);

			return new URIEditorInput(uri, editorName);
		}
	}

}

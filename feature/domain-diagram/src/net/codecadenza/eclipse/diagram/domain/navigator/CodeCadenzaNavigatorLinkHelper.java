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

import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaDiagramEditorPlugin;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.emf.common.ui.URIEditorInput;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.emf.workspace.util.WorkspaceSynchronizer;
import org.eclipse.gef.EditPart;
import org.eclipse.gef.GraphicalViewer;
import org.eclipse.gmf.runtime.diagram.ui.parts.DiagramEditor;
import org.eclipse.gmf.runtime.diagram.ui.resources.editor.document.IDiagramDocument;
import org.eclipse.gmf.runtime.notation.Diagram;
import org.eclipse.gmf.runtime.notation.View;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.navigator.ILinkHelper;
import org.eclipse.ui.part.FileEditorInput;

/**
 * <p>
 * Navigator link helper class
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CodeCadenzaNavigatorLinkHelper implements ILinkHelper {
	/**
	 * @param diagram
	 * @return the editor input
	 */
	private static IEditorInput getEditorInput(Diagram diagram) {
		final Resource diagramResource = diagram.eResource();

		for (final EObject nextEObject : diagramResource.getContents()) {
			if (nextEObject == diagram)
				return new FileEditorInput(WorkspaceSynchronizer.getFile(diagramResource));

			if (nextEObject instanceof Diagram)
				break;
		}

		final URI uri = EcoreUtil.getURI(diagram);
		final var editorName = uri.lastSegment() + "#" + diagram.eResource().getContents().indexOf(diagram);

		return new URIEditorInput(uri, editorName);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.navigator.ILinkHelper#findSelection(org.eclipse.ui.IEditorInput)
	 */
	@Override
	public IStructuredSelection findSelection(IEditorInput anInput) {
		final IDiagramDocument document = CodeCadenzaDiagramEditorPlugin.getInstance().getDocumentProvider()
				.getDiagramDocument(anInput);

		if (document == null)
			return StructuredSelection.EMPTY;

		final Diagram diagram = document.getDiagram();
		final IFile file = WorkspaceSynchronizer.getFile(diagram.eResource());

		if (file != null) {
			final var item = new CodeCadenzaNavigatorItem(diagram, file, false);
			return new StructuredSelection(item);
		}

		return StructuredSelection.EMPTY;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.navigator.ILinkHelper#activateEditor(org.eclipse.ui.IWorkbenchPage,
	 * org.eclipse.jface.viewers.IStructuredSelection)
	 */
	@Override
	public void activateEditor(IWorkbenchPage aPage, IStructuredSelection aSelection) {
		if (aSelection == null || aSelection.isEmpty())
			return;

		if (!(aSelection.getFirstElement() instanceof final CodeCadenzaAbstractNavigatorItem abstractNavigatorItem))
			return;

		View navigatorView = null;

		if (abstractNavigatorItem instanceof final CodeCadenzaNavigatorItem navigatorItem)
			navigatorView = navigatorItem.getView();
		else if (abstractNavigatorItem instanceof final CodeCadenzaNavigatorGroup navigatorGroup) {
			if (navigatorGroup.getParent() instanceof final CodeCadenzaNavigatorItem navigatorItem)
				navigatorView = navigatorItem.getView();
			else if (navigatorGroup.getParent() instanceof final IAdaptable adaptable)
				navigatorView = adaptable.getAdapter(View.class);
		}

		if (navigatorView == null)
			return;

		final IEditorInput editorInput = getEditorInput(navigatorView.getDiagram());
		final IEditorPart editor = aPage.findEditor(editorInput);

		if (editor == null)
			return;

		aPage.bringToTop(editor);

		if (editor instanceof final DiagramEditor diagramEditor) {
			final ResourceSet diagramEditorResourceSet = diagramEditor.getEditingDomain().getResourceSet();
			final EObject selectedView = diagramEditorResourceSet.getEObject(EcoreUtil.getURI(navigatorView), true);

			if (selectedView == null)
				return;

			final var graphicalViewer = (GraphicalViewer) diagramEditor.getAdapter(GraphicalViewer.class);
			final EditPart selectedEditPart = graphicalViewer.getEditPartRegistry().get(selectedView);

			if (selectedEditPart != null)
				graphicalViewer.select(selectedEditPart);
		}
	}

}

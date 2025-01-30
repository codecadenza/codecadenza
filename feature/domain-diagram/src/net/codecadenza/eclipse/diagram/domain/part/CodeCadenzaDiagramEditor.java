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

import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import net.codecadenza.eclipse.diagram.domain.navigator.CodeCadenzaNavigatorItem;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.emf.common.ui.URIEditorInput;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.edit.ui.dnd.LocalTransfer;
import org.eclipse.emf.transaction.TransactionalEditingDomain;
import org.eclipse.emf.workspace.util.WorkspaceSynchronizer;
import org.eclipse.gef.EditPartViewer;
import org.eclipse.gef.palette.PaletteRoot;
import org.eclipse.gef.ui.actions.ActionRegistry;
import org.eclipse.gmf.runtime.common.ui.services.marker.MarkerNavigationService;
import org.eclipse.gmf.runtime.diagram.core.preferences.PreferencesHint;
import org.eclipse.gmf.runtime.diagram.ui.actions.ActionIds;
import org.eclipse.gmf.runtime.diagram.ui.parts.DiagramDropTargetListener;
import org.eclipse.gmf.runtime.diagram.ui.properties.views.IReadOnlyDiagramPropertySheetPageContributor;
import org.eclipse.gmf.runtime.diagram.ui.resources.editor.document.IDiagramDocument;
import org.eclipse.gmf.runtime.diagram.ui.resources.editor.document.IDocument;
import org.eclipse.gmf.runtime.diagram.ui.resources.editor.document.IDocumentProvider;
import org.eclipse.gmf.runtime.diagram.ui.resources.editor.parts.DiagramDocumentEditor;
import org.eclipse.gmf.runtime.notation.Diagram;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.util.LocalSelectionTransfer;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.dnd.TransferData;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorMatchingStrategy;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.dialogs.SaveAsDialog;
import org.eclipse.ui.ide.IGotoMarker;
import org.eclipse.ui.navigator.resources.ProjectExplorer;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.ui.part.IShowInTargetList;
import org.eclipse.ui.part.ShowInContext;

/**
 * <p>
 * Diagram editor
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CodeCadenzaDiagramEditor extends DiagramDocumentEditor
		implements IReadOnlyDiagramPropertySheetPageContributor, IGotoMarker {
	public static final String ID = "net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaDiagramEditorID";
	public static final String CONTEXT_ID = "net.codecadenza.eclipse.diagram.domain.ui.diagramContext";

	/**
	 * Constructor
	 */
	public CodeCadenzaDiagramEditor() {
		super(true);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.parts.DiagramEditor#getContextID()
	 */
	@Override
	protected String getContextID() {
		return CONTEXT_ID;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.parts.DiagramEditorWithFlyOutPalette#createPaletteRoot(org.eclipse.gef.palette.
	 * PaletteRoot)
	 */
	@Override
	protected PaletteRoot createPaletteRoot(PaletteRoot existingPaletteRoot) {
		final PaletteRoot root = super.createPaletteRoot(existingPaletteRoot);
		new CodeCadenzaPaletteFactory().fillPalette(root);

		return root;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.parts.DiagramEditor#getPreferencesHint()
	 */
	@Override
	protected PreferencesHint getPreferencesHint() {
		return CodeCadenzaDiagramEditorPlugin.DIAGRAM_PREFERENCES_HINT;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.parts.DiagramEditor#getContributorId()
	 */
	@Override
	public String getContributorId() {
		return CodeCadenzaDiagramEditorPlugin.ID;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.resources.editor.parts.DiagramDocumentEditor#getAdapter(java.lang.Class)
	 */
	@Override
	public Object getAdapter(Class type) {
		if (type == IShowInTargetList.class)
			return (IShowInTargetList) () -> new String[] { ProjectExplorer.VIEW_ID };

		return super.getAdapter(type);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.resources.editor.parts.DiagramDocumentEditor#getDocumentProvider(org.eclipse.ui.
	 * IEditorInput)
	 */
	@Override
	protected IDocumentProvider getDocumentProvider(IEditorInput input) {
		if (input instanceof IFileEditorInput || input instanceof URIEditorInput)
			return CodeCadenzaDiagramEditorPlugin.getInstance().getDocumentProvider();

		return super.getDocumentProvider(input);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.resources.editor.parts.DiagramDocumentEditor#getEditingDomain()
	 */
	@Override
	public TransactionalEditingDomain getEditingDomain() {
		final IDocument document = getEditorInput() != null ? getDocumentProvider().getDocument(getEditorInput()) : null;

		if (document instanceof final IDiagramDocument diagramDocument)
			return diagramDocument.getEditingDomain();

		return super.getEditingDomain();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.resources.editor.parts.DiagramDocumentEditor#setDocumentProvider(org.eclipse.ui.
	 * IEditorInput)
	 */
	@Override
	protected void setDocumentProvider(IEditorInput input) {
		if (input instanceof IFileEditorInput || input instanceof URIEditorInput)
			setDocumentProvider(CodeCadenzaDiagramEditorPlugin.getInstance().getDocumentProvider());
		else
			super.setDocumentProvider(input);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.ide.IGotoMarker#gotoMarker(org.eclipse.core.resources.IMarker)
	 */
	@Override
	public void gotoMarker(IMarker marker) {
		MarkerNavigationService.getInstance().gotoMarker(this, marker);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.resources.editor.parts.DiagramDocumentEditor#isSaveAsAllowed()
	 */
	@Override
	public boolean isSaveAsAllowed() {
		return true;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.resources.editor.parts.DiagramDocumentEditor#doSaveAs()
	 */
	@Override
	public void doSaveAs() {
		performSaveAs(new NullProgressMonitor());
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.resources.editor.parts.DiagramDocumentEditor#performSaveAs(org.eclipse.core.runtime.
	 * IProgressMonitor)
	 */
	@Override
	protected void performSaveAs(IProgressMonitor progressMonitor) {
		final Shell shell = getSite().getShell();
		final IEditorInput input = getEditorInput();
		final var dialog = new SaveAsDialog(shell);
		final IFile original = input instanceof final IFileEditorInput fileEditorInput ? fileEditorInput.getFile() : null;

		if (original != null)
			dialog.setOriginalFile(original);

		dialog.create();

		final IDocumentProvider provider = getDocumentProvider();

		if (provider == null) {
			// Editor has been programmatically closed while the dialog was open
			return;
		}

		if (provider.isDeleted(input) && original != null) {
			final String message = NLS.bind(Messages.CodeCadenzaDiagramEditor_SavingDeletedFile, original.getName());
			dialog.setErrorMessage(null);
			dialog.setMessage(message, IMessageProvider.WARNING);
		}

		if (dialog.open() == Window.CANCEL) {
			if (progressMonitor != null)
				progressMonitor.setCanceled(true);

			return;
		}

		final IPath filePath = dialog.getResult();

		if (filePath == null) {
			if (progressMonitor != null)
				progressMonitor.setCanceled(true);

			return;
		}

		final IWorkspaceRoot workspaceRoot = ResourcesPlugin.getWorkspace().getRoot();
		final IFile file = workspaceRoot.getFile(filePath);
		final var newInput = new FileEditorInput(file);

		// Check if the editor is already open
		final IEditorMatchingStrategy matchingStrategy = getEditorDescriptor().getEditorMatchingStrategy();
		final IEditorReference[] editorRefs = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage()
				.getEditorReferences();

		for (final IEditorReference editorRef : editorRefs)
			if (matchingStrategy.matches(editorRef, newInput)) {
				MessageDialog.openWarning(shell, Messages.CodeCadenzaDiagramEditor_SaveAsErrorTitle,
						Messages.CodeCadenzaDiagramEditor_SaveAsErrorMessage);
				return;
			}

		boolean success = false;

		try {
			provider.aboutToChange(newInput);
			getDocumentProvider(newInput).saveDocument(progressMonitor, newInput, getDocumentProvider().getDocument(getEditorInput()),
					true);
			success = true;
		}
		catch (final CoreException x) {
			final IStatus status = x.getStatus();

			if (status == null || status.getSeverity() != IStatus.CANCEL)
				ErrorDialog.openError(shell, Messages.CodeCadenzaDiagramEditor_SaveErrorTitle,
						Messages.CodeCadenzaDiagramEditor_SaveErrorMessage, x.getStatus());
		}
		finally {
			provider.changed(newInput);

			if (success)
				setInput(newInput);
		}

		if (progressMonitor != null)
			progressMonitor.setCanceled(!success);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.parts.DiagramEditor#getShowInContext()
	 */
	@Override
	public ShowInContext getShowInContext() {
		return new ShowInContext(getEditorInput(), getNavigatorSelection());
	}

	/**
	 * @return the navigator selection
	 */
	private ISelection getNavigatorSelection() {
		final IDiagramDocument document = getDiagramDocument();

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
	 * @see org.eclipse.gmf.runtime.diagram.ui.parts.DiagramEditor#configureGraphicalViewer()
	 */
	@Override
	protected void configureGraphicalViewer() {
		super.configureGraphicalViewer();

		final var provider = new DiagramEditorContextMenuProvider(this, getDiagramGraphicalViewer());
		getDiagramGraphicalViewer().setContextMenu(provider);
		getSite().registerContextMenu(ActionIds.DIAGRAM_EDITOR_CONTEXT_MENU, provider, getDiagramGraphicalViewer());
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.resources.editor.parts.DiagramDocumentEditor#initializeGraphicalViewer()
	 */
	@Override
	protected void initializeGraphicalViewer() {
		super.initializeGraphicalViewer();

		getDiagramGraphicalViewer()
				.addDropTargetListener(new DropTargetListener(getDiagramGraphicalViewer(), LocalSelectionTransfer.getTransfer()) {
					/*
					 * (non-Javadoc)
					 * @see
					 * net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaDiagramEditor.DropTargetListener#getJavaObject(org.eclipse.
					 * swt.dnd.TransferData)
					 */
					@Override
					protected Object getJavaObject(TransferData data) {
						return LocalSelectionTransfer.getTransfer().nativeToJava(data);
					}

				});

		getDiagramGraphicalViewer()
				.addDropTargetListener(new DropTargetListener(getDiagramGraphicalViewer(), LocalTransfer.getInstance()) {
					/*
					 * (non-Javadoc)
					 * @see
					 * net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaDiagramEditor.DropTargetListener#getJavaObject(org.eclipse.
					 * swt.dnd.TransferData)
					 */
					@Override
					protected Object getJavaObject(TransferData data) {
						return LocalTransfer.getInstance().nativeToJava(data);
					}
				});
	}

	private abstract class DropTargetListener extends DiagramDropTargetListener {
		/**
		 * @param viewer
		 * @param xfer
		 */
		public DropTargetListener(EditPartViewer viewer, Transfer xfer) {
			super(viewer, xfer);
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.gmf.runtime.diagram.ui.parts.DiagramDropTargetListener#getObjectsBeingDropped()
		 */
		@Override
		protected List<EObject> getObjectsBeingDropped() {
			final TransferData data = getCurrentEvent().currentDataType;
			final var uris = new HashSet<URI>();
			final Object transferedObject = getJavaObject(data);

			if (transferedObject instanceof final IStructuredSelection selection) {
				for (Object nextSelectedObject : selection) {
					if (nextSelectedObject instanceof final CodeCadenzaNavigatorItem navigatorItem)
						nextSelectedObject = navigatorItem.getView().getElement();
					else if (nextSelectedObject instanceof final IAdaptable adaptable)
						nextSelectedObject = adaptable.getAdapter(EObject.class);

					if (nextSelectedObject instanceof final EObject modelElement) {
						final Resource modelElementResource = modelElement.eResource();
						uris.add(modelElementResource.getURI().appendFragment(modelElementResource.getURIFragment(modelElement)));
					}
				}
			}

			return uris.stream().map(uri -> getEditingDomain().getResourceSet().getEObject(uri, true)).toList();
		}

		protected abstract Object getJavaObject(TransferData data);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gef.ui.parts.GraphicalEditor#getActionRegistry()
	 */
	@Override
	protected ActionRegistry getActionRegistry() {
		final ActionRegistry x = super.getActionRegistry();
		final IAction action = x.getAction(ActionFactory.DELETE.getId());

		for (final Iterator<?> it = x.getActions(); it.hasNext();) {
			final Object o = it.next();

			if (action != null && action.equals(o)) {
				it.remove();
				break;
			}
		}

		return x;
	}

}

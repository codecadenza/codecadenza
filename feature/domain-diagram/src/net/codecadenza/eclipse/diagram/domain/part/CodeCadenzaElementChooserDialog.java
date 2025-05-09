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
import static net.codecadenza.eclipse.shared.Constants.MODEL_FILE_EXTENSION;

import java.util.Collections;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.common.util.WrappedException;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.emf.ecore.util.FeatureMap;
import org.eclipse.emf.edit.provider.IWrapperItemProvider;
import org.eclipse.emf.edit.ui.provider.AdapterFactoryContentProvider;
import org.eclipse.emf.edit.ui.provider.AdapterFactoryLabelProvider;
import org.eclipse.emf.transaction.TransactionalEditingDomain;
import org.eclipse.gmf.runtime.diagram.core.services.ViewService;
import org.eclipse.gmf.runtime.diagram.core.util.ViewUtil;
import org.eclipse.gmf.runtime.emf.core.GMFEditingDomainFactory;
import org.eclipse.gmf.runtime.emf.core.util.EObjectAdapter;
import org.eclipse.gmf.runtime.notation.Node;
import org.eclipse.gmf.runtime.notation.View;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.model.WorkbenchContentProvider;
import org.eclipse.ui.model.WorkbenchLabelProvider;

/**
 * <p>
 * Element chooser dialog
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CodeCadenzaElementChooserDialog extends Dialog {
	private URI selectedModelElementURI;
	private final View myView;
	private final TransactionalEditingDomain myEditingDomain = GMFEditingDomainFactory.INSTANCE.createEditingDomain();

	/**
	 * @param parentShell
	 * @param view
	 */
	public CodeCadenzaElementChooserDialog(Shell parentShell, View view) {
		super(parentShell);

		setShellStyle(getShellStyle() | SWT.RESIZE);
		myView = view;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent);
		getShell().setText(Messages.CodeCadenzaElementChooserDialog_SelectModelElementTitle);
		createModelBrowser(panDialogArea);

		return panDialogArea;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createButtonBar(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createButtonBar(Composite parent) {
		final Control buttonBar = super.createButtonBar(parent);
		setOkButtonEnabled(false);

		return buttonBar;
	}

	/**
	 * @param composite
	 */
	private void createModelBrowser(Composite composite) {
		final var glTreeViewer = new GridData(SWT.FILL, SWT.FILL, true, true);
		glTreeViewer.heightHint = 300;
		glTreeViewer.widthHint = 300;

		final var treeViewer = new TreeViewer(composite, SWT.SINGLE | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER);
		treeViewer.getTree().setLayoutData(glTreeViewer);
		treeViewer.setContentProvider(new ModelElementsTreeContentProvider());
		treeViewer.setLabelProvider(new ModelElementsTreeLabelProvider());
		treeViewer.setInput(ResourcesPlugin.getWorkspace().getRoot());
		treeViewer.addFilter(new ModelFilesFilter());
		treeViewer.addSelectionChangedListener(new OkButtonEnabler());
	}

	/**
	 * @param enabled
	 */
	private void setOkButtonEnabled(boolean enabled) {
		getButton(IDialogConstants.OK_ID).setEnabled(enabled);
	}

	/**
	 * @param file
	 * @return true if the file is valid
	 */
	private boolean isValidModelFile(IFile file) {
		final String fileExtension = file.getFullPath().getFileExtension();
		return DIAGRAM_FILE_EXTENSION.equals(fileExtension) || MODEL_FILE_EXTENSION.equals(fileExtension);
	}

	/**
	 * @return the model element URI
	 */
	public URI getSelectedModelElementURI() {
		return selectedModelElementURI;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.window.Window#open()
	 */
	@Override
	public int open() {
		final int result = super.open();

		myEditingDomain.getResourceSet().getResources().forEach(Resource::unload);
		myEditingDomain.dispose();

		return result;
	}

	private class ModelElementsTreeContentProvider implements ITreeContentProvider {
		private final ITreeContentProvider myWorkbenchContentProvider = new WorkbenchContentProvider();
		private final AdapterFactoryContentProvider myAdapterFctoryContentProvier = new AdapterFactoryContentProvider(
				CodeCadenzaDiagramEditorPlugin.getInstance().getItemProvidersAdapterFactory());

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ITreeContentProvider#getChildren(java.lang.Object)
		 */
		@Override
		public Object[] getChildren(Object parentElement) {
			final Object[] result = myWorkbenchContentProvider.getChildren(parentElement);

			if (result != null && result.length > 0)
				return result;

			if (parentElement instanceof final IFile modelFile) {
				final IPath resourcePath = modelFile.getFullPath();
				final ResourceSet resourceSet = myEditingDomain.getResourceSet();

				try {
					final Resource modelResource = resourceSet.getResource(URI.createPlatformResourceURI(resourcePath.toString(), true),
							true);
					return myAdapterFctoryContentProvier.getChildren(modelResource);
				}
				catch (final WrappedException e) {
					CodeCadenzaDiagramEditorPlugin.getInstance().logError("Unable to load resource: " + resourcePath.toString(), e);
				}

				return Collections.emptyList().toArray();
			}

			return myAdapterFctoryContentProvier.getChildren(parentElement);
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ITreeContentProvider#getParent(java.lang.Object)
		 */
		@Override
		public Object getParent(Object element) {
			final Object parent = myWorkbenchContentProvider.getParent(element);

			if (parent != null)
				return parent;

			if (element instanceof final EObject eObject) {
				if (eObject.eContainer() == null && eObject.eResource().getURI().isFile()) {
					final String path = eObject.eResource().getURI().path();
					return ResourcesPlugin.getWorkspace().getRoot().getFileForLocation(new Path(path));
				}

				return myAdapterFctoryContentProvier.getParent(eObject);
			}

			return null;
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ITreeContentProvider#hasChildren(java.lang.Object)
		 */
		@Override
		public boolean hasChildren(Object element) {
			if (element instanceof final IFile file)
				return isValidModelFile(file);

			return myWorkbenchContentProvider.hasChildren(element) || myAdapterFctoryContentProvier.hasChildren(element);
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ITreeContentProvider#getElements(java.lang.Object)
		 */
		@Override
		public Object[] getElements(Object inputElement) {
			return myWorkbenchContentProvider.getElements(inputElement);
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.IContentProvider#dispose()
		 */
		@Override
		public void dispose() {
			myWorkbenchContentProvider.dispose();
			myAdapterFctoryContentProvier.dispose();
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object,
		 * java.lang.Object)
		 */
		@Override
		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
			myWorkbenchContentProvider.inputChanged(viewer, oldInput, newInput);
			myAdapterFctoryContentProvier.inputChanged(viewer, oldInput, newInput);
		}
	}

	private static class ModelElementsTreeLabelProvider implements ILabelProvider {
		private final WorkbenchLabelProvider myWorkbenchLabelProvider = new WorkbenchLabelProvider();
		private final AdapterFactoryLabelProvider myAdapterFactoryLabelProvider = new AdapterFactoryLabelProvider(
				CodeCadenzaDiagramEditorPlugin.getInstance().getItemProvidersAdapterFactory());

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ILabelProvider#getImage(java.lang.Object)
		 */
		@Override
		public Image getImage(Object element) {
			final Image result = myWorkbenchLabelProvider.getImage(element);
			return result != null ? result : myAdapterFactoryLabelProvider.getImage(element);
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ILabelProvider#getText(java.lang.Object)
		 */
		@Override
		public String getText(Object element) {
			final String result = myWorkbenchLabelProvider.getText(element);
			return result != null && !result.isEmpty() ? result : myAdapterFactoryLabelProvider.getText(element);
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.IBaseLabelProvider#addListener(org.eclipse.jface.viewers.ILabelProviderListener)
		 */
		@Override
		public void addListener(ILabelProviderListener listener) {
			myWorkbenchLabelProvider.addListener(listener);
			myAdapterFactoryLabelProvider.addListener(listener);
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.IBaseLabelProvider#dispose()
		 */
		@Override
		public void dispose() {
			myWorkbenchLabelProvider.dispose();
			myAdapterFactoryLabelProvider.dispose();
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.IBaseLabelProvider#isLabelProperty(java.lang.Object, java.lang.String)
		 */
		@Override
		public boolean isLabelProperty(Object element, String property) {
			return myWorkbenchLabelProvider.isLabelProperty(element, property)
					|| myAdapterFactoryLabelProvider.isLabelProperty(element, property);
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.IBaseLabelProvider#removeListener(org.eclipse.jface.viewers.ILabelProviderListener)
		 */
		@Override
		public void removeListener(ILabelProviderListener listener) {
			myWorkbenchLabelProvider.removeListener(listener);
			myAdapterFactoryLabelProvider.removeListener(listener);
		}
	}

	private class ModelFilesFilter extends ViewerFilter {
		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ViewerFilter#select(org.eclipse.jface.viewers.Viewer, java.lang.Object, java.lang.Object)
		 */
		@Override
		public boolean select(Viewer viewer, Object parentElement, Object element) {
			if (element instanceof IContainer)
				return true;

			if (element instanceof final IFile file)
				return isValidModelFile(file);

			return true;
		}
	}

	private class OkButtonEnabler implements ISelectionChangedListener {
		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
		 */
		@Override
		public void selectionChanged(SelectionChangedEvent event) {
			if (event.getSelection() instanceof final IStructuredSelection selection && selection.size() == 1) {
				Object selectedElement = selection.getFirstElement();

				if (selectedElement instanceof final IWrapperItemProvider itemProvider)
					selectedElement = itemProvider.getValue();

				if (selectedElement instanceof final FeatureMap.Entry entry)
					selectedElement = entry.getValue();

				if (selectedElement instanceof final EObject selectedModelElement) {
					setOkButtonEnabled(ViewService.getInstance().provides(Node.class, new EObjectAdapter(selectedModelElement), myView,
							null, ViewUtil.APPEND, true, CodeCadenzaDiagramEditorPlugin.DIAGRAM_PREFERENCES_HINT));
					selectedModelElementURI = EcoreUtil.getURI(selectedModelElement);

					return;
				}
			}

			selectedModelElementURI = null;
			setOkButtonEnabled(false);
		}
	}

}

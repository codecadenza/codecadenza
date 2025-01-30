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

import java.util.ArrayList;
import java.util.HashMap;
import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaDiagramEditorPlugin;
import org.eclipse.core.resources.IFile;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.edit.domain.AdapterFactoryEditingDomain;
import org.eclipse.emf.edit.ui.provider.AdapterFactoryContentProvider;
import org.eclipse.emf.transaction.TransactionalEditingDomain;
import org.eclipse.emf.workspace.util.WorkspaceSynchronizer;
import org.eclipse.gmf.runtime.emf.core.GMFEditingDomainFactory;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.navigator.ICommonContentExtensionSite;
import org.eclipse.ui.navigator.ICommonContentProvider;

/**
 * <p>
 * Domain navigator content provider
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CodeCadenzaDomainNavigatorContentProvider implements ICommonContentProvider {
	private AdapterFactoryContentProvider myAdapterFctoryContentProvier;
	private static final Object[] EMPTY_ARRAY = {};
	private Viewer myViewer;
	private AdapterFactoryEditingDomain myEditingDomain;
	private WorkspaceSynchronizer myWorkspaceSynchronizer;
	private Runnable myViewerRefreshRunnable;

	/**
	 * Constructor
	 */
	public CodeCadenzaDomainNavigatorContentProvider() {
		myAdapterFctoryContentProvier = new AdapterFactoryContentProvider(
				CodeCadenzaDiagramEditorPlugin.getInstance().getItemProvidersAdapterFactory());

		final TransactionalEditingDomain editingDomain = GMFEditingDomainFactory.INSTANCE.createEditingDomain();

		myEditingDomain = (AdapterFactoryEditingDomain) editingDomain;

		myEditingDomain.setResourceToReadOnlyMap(new HashMap<Resource, Boolean>() {
			private static final long serialVersionUID = 6265439791308848737L;

			/*
			 * (non-Javadoc)
			 * @see java.util.HashMap#get(java.lang.Object)
			 */
			@Override
			public Boolean get(Object key) {
				if (!containsKey(key))
					put((Resource) key, Boolean.TRUE);

				return super.get(key);
			}
		});

		myViewerRefreshRunnable = () -> {
			if (myViewer != null)
				myViewer.refresh();
		};

		myWorkspaceSynchronizer = new WorkspaceSynchronizer(editingDomain, new WorkspaceSynchronizer.Delegate() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.emf.workspace.util.WorkspaceSynchronizer.Delegate#dispose()
			 */
			@Override
			public void dispose() {
				// No implementation required!
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.emf.workspace.util.WorkspaceSynchronizer.Delegate#handleResourceChanged(org.eclipse.emf.ecore.resource.
			 * Resource)
			 */
			@Override
			public boolean handleResourceChanged(final Resource resource) {
				unloadResources();

				return true;
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.emf.workspace.util.WorkspaceSynchronizer.Delegate#handleResourceDeleted(org.eclipse.emf.ecore.resource.
			 * Resource)
			 */
			@Override
			public boolean handleResourceDeleted(Resource resource) {
				return handleResourceChanged(resource);
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.emf.workspace.util.WorkspaceSynchronizer.Delegate#handleResourceMoved(org.eclipse.emf.ecore.resource.
			 * Resource, org.eclipse.emf.common.util.URI)
			 */
			@Override
			public boolean handleResourceMoved(Resource resource, final URI newURI) {
				return handleResourceChanged(resource);
			}

			/**
			 * Unload all resources
			 */
			private void unloadResources() {
				myEditingDomain.getResourceSet().getResources().forEach(Resource::unload);

				if (myViewer != null)
					myViewer.getControl().getDisplay().asyncExec(myViewerRefreshRunnable);
			}
		});
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.viewers.IContentProvider#dispose()
	 */
	@Override
	public void dispose() {
		myWorkspaceSynchronizer.dispose();
		myWorkspaceSynchronizer = null;
		myViewerRefreshRunnable = null;

		myEditingDomain.getResourceSet().getResources().forEach(Resource::unload);

		((TransactionalEditingDomain) myEditingDomain).dispose();
		myEditingDomain = null;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object,
	 * java.lang.Object)
	 */
	@Override
	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		myViewer = viewer;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.viewers.ITreeContentProvider#getElements(java.lang.Object)
	 */
	@Override
	public Object[] getElements(Object inputElement) {
		return getChildren(inputElement);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.navigator.IMementoAware#restoreState(org.eclipse.ui.IMemento)
	 */
	@Override
	public void restoreState(IMemento aMemento) {
		// No implementation required!
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.navigator.IMementoAware#saveState(org.eclipse.ui.IMemento)
	 */
	@Override
	public void saveState(IMemento aMemento) {
		// No implementation required!
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.navigator.ICommonContentProvider#init(org.eclipse.ui.navigator.ICommonContentExtensionSite)
	 */
	@Override
	public void init(ICommonContentExtensionSite aConfig) {
		// No implementation required!
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.viewers.ITreeContentProvider#getChildren(java.lang.Object)
	 */
	@Override
	public Object[] getChildren(Object parentElement) {
		if (parentElement instanceof final IFile file) {
			final URI fileURI = URI.createPlatformResourceURI(file.getFullPath().toString(), true);
			final Resource resource = myEditingDomain.getResourceSet().getResource(fileURI, true);

			return wrapEObjects(myAdapterFctoryContentProvier.getChildren(resource), parentElement);
		}

		if (parentElement instanceof final CodeCadenzaDomainNavigatorItem domainNavigatorItem)
			return wrapEObjects(myAdapterFctoryContentProvier.getChildren(domainNavigatorItem.getEObject()), parentElement);

		return EMPTY_ARRAY;
	}

	/**
	 * @param objects
	 * @param parentElement
	 * @return an array of objects
	 */
	public Object[] wrapEObjects(Object[] objects, Object parentElement) {
		final var result = new ArrayList<>();

		for (final Object object : objects)
			if (object instanceof final EObject eObject)
				result.add(new CodeCadenzaDomainNavigatorItem(eObject, parentElement, myAdapterFctoryContentProvier));

		return result.toArray();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.viewers.ITreeContentProvider#getParent(java.lang.Object)
	 */
	@Override
	public Object getParent(Object element) {
		if (element instanceof final CodeCadenzaAbstractNavigatorItem abstractNavigatorItem) {
			return abstractNavigatorItem.getParent();
		}

		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.viewers.ITreeContentProvider#hasChildren(java.lang.Object)
	 */
	@Override
	public boolean hasChildren(Object element) {
		return element instanceof IFile || getChildren(element).length > 0;
	}

}

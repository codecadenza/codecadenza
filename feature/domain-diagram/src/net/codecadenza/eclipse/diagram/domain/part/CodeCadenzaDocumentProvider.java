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

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import net.codecadenza.eclipse.shared.event.GraphicalEditorEventController;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceStatus;
import org.eclipse.core.resources.IStorage;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.MultiRule;
import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.common.ui.URIEditorInput;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.util.EContentAdapter;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.emf.transaction.NotificationFilter;
import org.eclipse.emf.transaction.TransactionalEditingDomain;
import org.eclipse.emf.workspace.util.WorkspaceSynchronizer;
import org.eclipse.gmf.runtime.common.core.command.CommandResult;
import org.eclipse.gmf.runtime.diagram.core.DiagramEditingDomainFactory;
import org.eclipse.gmf.runtime.diagram.ui.resources.editor.document.AbstractDocumentProvider;
import org.eclipse.gmf.runtime.diagram.ui.resources.editor.document.DiagramDocument;
import org.eclipse.gmf.runtime.diagram.ui.resources.editor.document.IDiagramDocument;
import org.eclipse.gmf.runtime.diagram.ui.resources.editor.document.IDiagramDocumentProvider;
import org.eclipse.gmf.runtime.diagram.ui.resources.editor.document.IDocument;
import org.eclipse.gmf.runtime.diagram.ui.resources.editor.internal.EditorStatusCodes;
import org.eclipse.gmf.runtime.diagram.ui.resources.editor.internal.util.DiagramIOUtil;
import org.eclipse.gmf.runtime.emf.commands.core.command.AbstractTransactionalCommand;
import org.eclipse.gmf.runtime.emf.core.resources.GMFResourceFactory;
import org.eclipse.gmf.runtime.notation.Diagram;
import org.eclipse.jface.operation.IRunnableContext;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.part.FileEditorInput;

/**
 * <p>
 * Document provider
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
@SuppressWarnings("restriction")
public class CodeCadenzaDocumentProvider extends AbstractDocumentProvider implements IDiagramDocumentProvider {
	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.resources.editor.document.AbstractDocumentProvider#createElementInfo(java.lang.
	 * Object)
	 */
	@Override
	protected ElementInfo createElementInfo(Object element) throws CoreException {
		if (!(element instanceof FileEditorInput) && !(element instanceof URIEditorInput))
			throw new CoreException(new Status(IStatus.ERROR, CodeCadenzaDiagramEditorPlugin.ID, 0,
					NLS.bind(Messages.CodeCadenzaDocumentProvider_IncorrectInputError,
							new Object[] { element, "org.eclipse.ui.part.FileEditorInput", "org.eclipse.emf.common.ui.URIEditorInput" }),
					null));

		final var editorInput = (IEditorInput) element;
		final var document = (IDiagramDocument) createDocument(editorInput);

		final var info = new ResourceSetInfo(document, editorInput);
		info.setModificationStamp(computeModificationStamp(info));
		info.fStatus = null;

		return info;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.resources.editor.document.AbstractDocumentProvider#createDocument(java.lang.Object)
	 */
	@Override
	protected IDocument createDocument(Object element) throws CoreException {
		if (!(element instanceof FileEditorInput) && !(element instanceof URIEditorInput))
			throw new CoreException(new Status(IStatus.ERROR, CodeCadenzaDiagramEditorPlugin.ID, 0,
					NLS.bind(Messages.CodeCadenzaDocumentProvider_IncorrectInputError,
							new Object[] { element, "org.eclipse.ui.part.FileEditorInput", "org.eclipse.emf.common.ui.URIEditorInput" }),
					null));

		final IDocument document = createEmptyDocument();
		setDocumentContent(document, (IEditorInput) element);
		setupDocument(element, document);

		return document;
	}

	/**
	 * Set up the given document as it would be provided for the given element. The content of the document is not changed. This
	 * default implementation is empty.
	 * @param element
	 * @param document
	 */
	@SuppressWarnings("unused")
	protected void setupDocument(Object element, IDocument document) {
		// No implementation required!
	}

	/**
	 * @param info
	 * @return the modification stamp
	 */
	private long computeModificationStamp(ResourceSetInfo info) {
		int result = 0;

		for (final Iterator<Resource> it = info.getLoadedResourcesIterator(); it.hasNext();) {
			final Resource nextResource = it.next();
			final IFile file = WorkspaceSynchronizer.getFile(nextResource);

			if (file != null) {
				if (file.getLocation() != null)
					result += file.getLocation().toFile().lastModified();
				else
					result += file.getModificationStamp();
			}
		}

		return result;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.resources.editor.document.AbstractDocumentProvider#createEmptyDocument()
	 */
	@Override
	protected IDocument createEmptyDocument() {
		final var document = new DiagramDocument();
		document.setEditingDomain(createEditingDomain());

		return document;
	}

	/**
	 * @return the editing domain
	 */
	private TransactionalEditingDomain createEditingDomain() {
		final TransactionalEditingDomain editingDomain = DiagramEditingDomainFactory.getInstance().createEditingDomain();
		editingDomain.setID("net.codecadenza.eclipse.diagram.EditingDomain");
		final NotificationFilter diagramResourceModifiedFilter = NotificationFilter
				.createNotifierFilter(editingDomain.getResourceSet()).and(NotificationFilter.createEventTypeFilter(Notification.ADD))
				.and(NotificationFilter.createFeatureFilter(ResourceSet.class, ResourceSet.RESOURCE_SET__RESOURCES));

		editingDomain.getResourceSet().eAdapters().add(new Adapter() {
			private Notifier myTarger;

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.emf.common.notify.Adapter#getTarget()
			 */
			@Override
			public Notifier getTarget() {
				return myTarger;
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.emf.common.notify.Adapter#isAdapterForType(java.lang.Object)
			 */
			@Override
			public boolean isAdapterForType(Object type) {
				return false;
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.emf.common.notify.Adapter#notifyChanged(org.eclipse.emf.common.notify.Notification)
			 */
			@Override
			public void notifyChanged(Notification notification) {
				if (diagramResourceModifiedFilter.matches(notification)) {
					final Object value = notification.getNewValue();

					if (value instanceof final Resource resource)
						resource.setTrackingModification(true);
				}
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.emf.common.notify.Adapter#setTarget(org.eclipse.emf.common.notify.Notifier)
			 */
			@Override
			public void setTarget(Notifier newTarget) {
				myTarger = newTarget;
			}
		});

		return editingDomain;
	}

	/**
	 * @param document
	 * @param element
	 * @throws CoreException if an internal error has occurred
	 */
	protected void setDocumentContent(IDocument document, IEditorInput element) throws CoreException {
		final var diagramDocument = (IDiagramDocument) document;
		final TransactionalEditingDomain domain = diagramDocument.getEditingDomain();

		if (element instanceof final FileEditorInput fileEditorInput) {
			final IStorage storage = fileEditorInput.getStorage();
			final Diagram diagram = DiagramIOUtil.load(domain, storage, true, getProgressMonitor());
			document.setContent(diagram);
		}
		else if (element instanceof final URIEditorInput uriEditorInput) {
			final URI uri = uriEditorInput.getURI();
			Resource resource = null;

			try {
				resource = domain.getResourceSet().getResource(uri.trimFragment(), false);

				if (resource == null)
					resource = domain.getResourceSet().createResource(uri.trimFragment());

				loadResource(resource);

				if (uri.fragment() != null) {
					final EObject rootElement = resource.getEObject(uri.fragment());

					if (rootElement instanceof Diagram) {
						document.setContent(rootElement);
						return;
					}
				}
				else {
					for (final EObject eObject : resource.getContents()) {
						final Object rootElement = eObject;

						if (rootElement instanceof Diagram) {
							document.setContent(rootElement);
							return;
						}
					}
				}

				throw new RuntimeException(Messages.CodeCadenzaDocumentProvider_NoDiagramInResourceError);
			}
			catch (final Exception e) {
				CoreException thrownExcp = null;

				if (e instanceof final CoreException coreException)
					thrownExcp = coreException;
				else {
					final String msg = e.getLocalizedMessage();
					thrownExcp = new CoreException(new Status(IStatus.ERROR, CodeCadenzaDiagramEditorPlugin.ID, 0,
							msg != null ? msg : Messages.CodeCadenzaDocumentProvider_DiagramLoadingError, e));
				}

				throw thrownExcp;
			}
		}
		else {
			throw new CoreException(new Status(IStatus.ERROR, CodeCadenzaDiagramEditorPlugin.ID, 0,
					NLS.bind(Messages.CodeCadenzaDocumentProvider_IncorrectInputError,
							new Object[] { element, "org.eclipse.ui.part.FileEditorInput", "org.eclipse.emf.common.ui.URIEditorInput" }),
					null));
		}
	}

	/**
	 * Load the given resource
	 * @param resource
	 * @throws IOException if the resource could not be loaded
	 */
	@SuppressWarnings("unchecked")
	private void loadResource(Resource resource) throws IOException {
		if (resource.isLoaded())
			return;

		try {
			final var options = new HashMap<>(GMFResourceFactory.getDefaultLoadOptions());

			resource.load(options);
		}
		catch (final IOException e) {
			resource.unload();
			throw e;
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.resources.editor.document.AbstractDocumentProvider#getModificationStamp(java.lang.
	 * Object)
	 */
	@Override
	public long getModificationStamp(Object element) {
		final ResourceSetInfo info = getResourceSetInfo(element);

		if (info != null)
			return computeModificationStamp(info);

		return super.getModificationStamp(element);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.resources.editor.document.AbstractDocumentProvider#isDeleted(java.lang.Object)
	 */
	@Override
	public boolean isDeleted(Object element) {
		final IDiagramDocument document = getDiagramDocument(element);

		if (document != null) {
			final Resource diagramResource = document.getDiagram().eResource();

			if (diagramResource != null) {
				final IFile file = WorkspaceSynchronizer.getFile(diagramResource);
				return file == null || file.getLocation() == null || !file.getLocation().toFile().exists();
			}
		}

		return super.isDeleted(element);
	}

	/**
	 * @param editorInput
	 * @return the resource set info
	 */
	public ResourceSetInfo getResourceSetInfo(Object editorInput) {
		return (ResourceSetInfo) super.getElementInfo(editorInput);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.resources.editor.document.AbstractDocumentProvider#disposeElementInfo(java.lang.
	 * Object, org.eclipse.gmf.runtime.diagram.ui.resources.editor.document.AbstractDocumentProvider.ElementInfo)
	 */
	@Override
	protected void disposeElementInfo(Object element, ElementInfo info) {
		if (info instanceof final ResourceSetInfo resourceSetInfo)
			resourceSetInfo.dispose();

		super.disposeElementInfo(element, info);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.resources.editor.document.AbstractDocumentProvider#doValidateState(java.lang.Object,
	 * java.lang.Object)
	 */
	@Override
	protected void doValidateState(Object element, Object computationContext) throws CoreException {
		final ResourceSetInfo info = getResourceSetInfo(element);

		if (info != null) {
			final var files2Validate = new ArrayList<IFile>();

			for (final Iterator<Resource> it = info.getLoadedResourcesIterator(); it.hasNext();) {
				final Resource nextResource = it.next();
				final IFile file = WorkspaceSynchronizer.getFile(nextResource);

				if (file != null && file.isReadOnly())
					files2Validate.add(file);
			}

			ResourcesPlugin.getWorkspace().validateEdit(files2Validate.toArray(new IFile[files2Validate.size()]), computationContext);
		}

		super.doValidateState(element, computationContext);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.resources.editor.document.AbstractDocumentProvider#isReadOnly(java.lang.Object)
	 */
	@Override
	public boolean isReadOnly(Object element) {
		final ResourceSetInfo info = getResourceSetInfo(element);

		if (info != null) {
			if (info.isUpdateCache())
				try {
					updateCache(element);
				}
				catch (final Exception ex) {
					CodeCadenzaDiagramEditorPlugin.getInstance().logError(Messages.CodeCadenzaDocumentProvider_isModifiable, ex);
					// The error message to log was initially taken from
					// org.eclipse.gmf.runtime.diagram.ui.resources.editor.ide.internal.l10n.EditorMessages.StorageDocumentProvider_isModifiable
				}

			return info.isReadOnly();
		}

		return super.isReadOnly(element);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.resources.editor.document.AbstractDocumentProvider#isModifiable(java.lang.Object)
	 */
	@Override
	public boolean isModifiable(Object element) {
		if (!isStateValidated(element) && (element instanceof FileEditorInput || element instanceof URIEditorInput))
			return true;

		final ResourceSetInfo info = getResourceSetInfo(element);

		if (info != null) {
			if (info.isUpdateCache())
				try {
					updateCache(element);
				}
				catch (final Exception ex) {
					CodeCadenzaDiagramEditorPlugin.getInstance().logError(Messages.CodeCadenzaDocumentProvider_isModifiable, ex);
					// The error message to log was initially taken from
					// org.eclipse.gmf.runtime.diagram.ui.resources.editor.ide.internal.l10n.EditorMessages.StorageDocumentProvider_isModifiable
				}

			return info.isModifiable();
		}

		return super.isModifiable(element);
	}

	/**
	 * @param element
	 */
	protected void updateCache(Object element) {
		final ResourceSetInfo info = getResourceSetInfo(element);

		if (info != null) {
			for (final Iterator<Resource> it = info.getLoadedResourcesIterator(); it.hasNext();) {
				final Resource nextResource = it.next();
				final IFile file = WorkspaceSynchronizer.getFile(nextResource);

				if (file != null && file.isReadOnly()) {
					info.setReadOnly(true);
					info.setModifiable(false);
					return;
				}
			}

			info.setReadOnly(false);
			info.setModifiable(true);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.resources.editor.document.AbstractDocumentProvider#doUpdateStateCache(java.lang.
	 * Object)
	 */
	@Override
	protected void doUpdateStateCache(Object element) throws CoreException {
		final ResourceSetInfo info = getResourceSetInfo(element);

		if (info != null)
			info.setUpdateCache(true);

		super.doUpdateStateCache(element);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.resources.editor.document.AbstractDocumentProvider#isSynchronized(java.lang.Object)
	 */
	@Override
	public boolean isSynchronized(Object element) {
		final ResourceSetInfo info = getResourceSetInfo(element);

		if (info != null)
			return info.isSynchronized();

		return super.isSynchronized(element);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.resources.editor.document.AbstractDocumentProvider#getResetRule(java.lang.Object)
	 */
	@Override
	protected ISchedulingRule getResetRule(Object element) {
		final ResourceSetInfo info = getResourceSetInfo(element);

		if (info != null) {
			final var rules = new ArrayList<ISchedulingRule>();

			for (final Iterator<Resource> it = info.getLoadedResourcesIterator(); it.hasNext();) {
				final Resource nextResource = it.next();
				final IFile file = WorkspaceSynchronizer.getFile(nextResource);

				if (file != null)
					rules.add(ResourcesPlugin.getWorkspace().getRuleFactory().modifyRule(file));
			}

			return new MultiRule(rules.toArray(new ISchedulingRule[rules.size()]));
		}

		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.resources.editor.document.AbstractDocumentProvider#getSaveRule(java.lang.Object)
	 */
	@Override
	protected ISchedulingRule getSaveRule(Object element) {
		final ResourceSetInfo info = getResourceSetInfo(element);

		if (info != null) {
			final var rules = new ArrayList<ISchedulingRule>();

			for (final Iterator<Resource> it = info.getLoadedResourcesIterator(); it.hasNext();) {
				final Resource nextResource = it.next();
				final IFile file = WorkspaceSynchronizer.getFile(nextResource);

				if (file != null)
					rules.add(computeSchedulingRule(file));
			}

			return new MultiRule(rules.toArray(new ISchedulingRule[rules.size()]));
		}

		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.resources.editor.document.AbstractDocumentProvider#getSynchronizeRule(java.lang.
	 * Object)
	 */
	@Override
	protected ISchedulingRule getSynchronizeRule(Object element) {
		final ResourceSetInfo info = getResourceSetInfo(element);

		if (info != null) {
			final var rules = new ArrayList<ISchedulingRule>();

			for (final Iterator<Resource> it = info.getLoadedResourcesIterator(); it.hasNext();) {
				final Resource nextResource = it.next();
				final IFile file = WorkspaceSynchronizer.getFile(nextResource);

				if (file != null)
					rules.add(ResourcesPlugin.getWorkspace().getRuleFactory().refreshRule(file));
			}

			return new MultiRule(rules.toArray(new ISchedulingRule[rules.size()]));
		}

		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.resources.editor.document.AbstractDocumentProvider#getValidateStateRule(java.lang.
	 * Object)
	 */
	@Override
	protected ISchedulingRule getValidateStateRule(Object element) {
		final ResourceSetInfo info = getResourceSetInfo(element);

		if (info != null) {
			final var files = new ArrayList<ISchedulingRule>();

			for (final Iterator<Resource> it = info.getLoadedResourcesIterator(); it.hasNext();) {
				final Resource nextResource = it.next();
				final IFile file = WorkspaceSynchronizer.getFile(nextResource);

				if (file != null)
					files.add(file);
			}

			return ResourcesPlugin.getWorkspace().getRuleFactory().validateEditRule(files.toArray(new IFile[files.size()]));
		}

		return null;
	}

	/**
	 * @param toCreateOrModify
	 * @return a scheduling rule
	 */
	private ISchedulingRule computeSchedulingRule(IResource toCreateOrModify) {
		if (toCreateOrModify.exists())
			return ResourcesPlugin.getWorkspace().getRuleFactory().modifyRule(toCreateOrModify);

		IResource parent = toCreateOrModify;

		do {
			toCreateOrModify = parent;
			parent = toCreateOrModify.getParent();
		}
		while (parent != null && !parent.exists());

		return ResourcesPlugin.getWorkspace().getRuleFactory().createRule(toCreateOrModify);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.resources.editor.document.AbstractDocumentProvider#doSynchronize(java.lang.Object,
	 * org.eclipse.core.runtime.IProgressMonitor)
	 */
	@Override
	protected void doSynchronize(Object element, IProgressMonitor monitor) throws CoreException {
		final ResourceSetInfo info = getResourceSetInfo(element);

		if (info != null) {
			for (final Iterator<Resource> it = info.getLoadedResourcesIterator(); it.hasNext();) {
				final Resource nextResource = it.next();
				handleElementChanged(info, nextResource, monitor);
			}

			return;
		}

		super.doSynchronize(element, monitor);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.resources.editor.document.AbstractDocumentProvider#
	 * doSaveDocument(org.eclipse.core.runtime.IProgressMonitor, java.lang.Object,
	 * org.eclipse.gmf.runtime.diagram.ui.resources.editor.document.IDocument, boolean)
	 */
	@Override
	protected void doSaveDocument(IProgressMonitor monitor, Object element, IDocument document, boolean overwrite)
			throws CoreException {
		final ResourceSetInfo info = getResourceSetInfo(element);

		if (info != null) {
			if (!overwrite && !info.isSynchronized())
				throw new CoreException(new Status(IStatus.ERROR, CodeCadenzaDiagramEditorPlugin.ID, IResourceStatus.OUT_OF_SYNC_LOCAL,
						Messages.CodeCadenzaDocumentProvider_UnsynchronizedFileSaveError, null));

			info.stopResourceListening();
			fireElementStateChanging(element);

			try {
				// Saving diagram
				monitor.beginTask(Messages.CodeCadenzaDocumentProvider_SaveDiagramTask, info.getResourceSet().getResources().size() + 1);

				for (final Iterator<Resource> it = info.getLoadedResourcesIterator(); it.hasNext();) {
					final Resource nextResource = it.next();
					monitor.setTaskName(NLS.bind(Messages.CodeCadenzaDocumentProvider_SaveNextResourceTask, nextResource.getURI()));

					if (nextResource.isLoaded() && !info.getEditingDomain().isReadOnly(nextResource))
						saveResource(nextResource, element);

					monitor.worked(1);
				}

				monitor.done();
				info.setModificationStamp(computeModificationStamp(info));

				// Notify all registered listeners about this event!
				GraphicalEditorEventController.fireSaveEvent();
			}
			catch (final RuntimeException x) {
				fireElementStateChangeFailed(element);
				throw x;
			}
			finally {
				info.startResourceListening();
			}
		}
		else {
			URI newResoruceURI;
			List<IFile> affectedFiles = null;

			if (element instanceof final FileEditorInput fileEditorInput) {
				final IFile newFile = fileEditorInput.getFile();
				affectedFiles = Collections.singletonList(newFile);
				newResoruceURI = URI.createPlatformResourceURI(newFile.getFullPath().toString(), true);
			}
			else if (element instanceof final URIEditorInput uriEditorInput)
				newResoruceURI = uriEditorInput.getURI();
			else {
				fireElementStateChangeFailed(element);

				throw new CoreException(new Status(IStatus.ERROR, CodeCadenzaDiagramEditorPlugin.ID, 0,
						NLS.bind(Messages.CodeCadenzaDocumentProvider_IncorrectInputError,
								new Object[] { element, "org.eclipse.ui.part.FileEditorInput", "org.eclipse.emf.common.ui.URIEditorInput" }),
						null));
			}

			if (!(document instanceof final IDiagramDocument diagramDocument)) {
				fireElementStateChangeFailed(element);

				final var msg = "Incorrect document used: " + document + " instead of IDiagramDocument";

				throw new CoreException(new Status(IStatus.ERROR, CodeCadenzaDiagramEditorPlugin.ID, 0, msg, null));
			}

			final Resource newResource = diagramDocument.getEditingDomain().getResourceSet().createResource(newResoruceURI);
			final Diagram diagramCopy = EcoreUtil.copy(diagramDocument.getDiagram());

			try {
				new AbstractTransactionalCommand(diagramDocument.getEditingDomain(),
						NLS.bind(Messages.CodeCadenzaDocumentProvider_SaveAsOperation, diagramCopy.getName()), affectedFiles) {
					/*
					 * (non-Javadoc)
					 * @see org.eclipse.gmf.runtime.emf.commands.core.command.AbstractTransactionalCommand#doExecuteWithResult(org.eclipse.
					 * core.runtime.IProgressMonitor, org.eclipse.core.runtime.IAdaptable)
					 */
					@Override
					protected CommandResult doExecuteWithResult(IProgressMonitor monitor, IAdaptable info) throws ExecutionException {
						newResource.getContents().add(diagramCopy);
						return CommandResult.newOKCommandResult();
					}
				}.execute(monitor, null);

				newResource.save(CodeCadenzaDiagramEditorUtil.getSaveOptions());
			}
			catch (final IOException | ExecutionException e) {
				fireElementStateChangeFailed(element);
				throw new CoreException(new Status(IStatus.ERROR, CodeCadenzaDiagramEditorPlugin.ID, 0, e.getLocalizedMessage(), null));
			}

			newResource.unload();
		}
	}

	/**
	 * Save the given resource
	 * @param resource
	 * @param element
	 * @throws CoreException if the save operation has failed
	 */
	private void saveResource(Resource resource, Object element) throws CoreException {
		try {
			resource.save(CodeCadenzaDiagramEditorUtil.getSaveOptions());
		}
		catch (final IOException e) {
			fireElementStateChangeFailed(element);
			throw new CoreException(new Status(IStatus.ERROR, CodeCadenzaDiagramEditorPlugin.ID, EditorStatusCodes.RESOURCE_FAILURE,
					e.getLocalizedMessage(), null));
		}
	}

	/**
	 * @param info
	 * @param changedResource
	 * @param monitor
	 */
	protected void handleElementChanged(ResourceSetInfo info, Resource changedResource, IProgressMonitor monitor) {
		final IFile file = WorkspaceSynchronizer.getFile(changedResource);

		if (file != null) {
			try {
				file.refreshLocal(IResource.DEPTH_INFINITE, monitor);
			}
			catch (final CoreException ex) {
				CodeCadenzaDiagramEditorPlugin.getInstance().logError(Messages.CodeCadenzaDocumentProvider_handleElementContentChanged,
						ex);
				// The error message to log was initially taken from
				// org.eclipse.gmf.runtime.diagram.ui.resources.editor.ide.internal.l10n.EditorMessages.FileDocumentProvider_handleElementContentChanged
			}
		}

		changedResource.unload();

		fireElementContentAboutToBeReplaced(info.getEditorInput());
		removeUnchangedElementListeners(info.getEditorInput(), info);
		info.fStatus = null;

		try {
			setDocumentContent(info.fDocument, info.getEditorInput());
		}
		catch (final CoreException e) {
			info.fStatus = e.getStatus();
		}

		if (!info.fCanBeSaved)
			info.setModificationStamp(computeModificationStamp(info));

		addUnchangedElementListeners(info.getEditorInput(), info);
		fireElementContentReplaced(info.getEditorInput());
	}

	/**
	 * @param input
	 * @param uri
	 */
	protected void handleElementMoved(IEditorInput input, URI uri) {
		if (input instanceof FileEditorInput) {
			final IFile newFile = ResourcesPlugin.getWorkspace().getRoot()
					.getFile(new Path(URI.decode(uri.path())).removeFirstSegments(1));
			fireElementMoved(input, newFile == null ? null : new FileEditorInput(newFile));

			return;
		}

		fireElementMoved(input, new URIEditorInput(uri));
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.resources.editor.document.IDiagramDocumentProvider#
	 * createInputWithEditingDomain(org.eclipse.ui.IEditorInput, org.eclipse.emf.transaction.TransactionalEditingDomain)
	 */
	@Override
	public IEditorInput createInputWithEditingDomain(IEditorInput editorInput, TransactionalEditingDomain domain) {
		return editorInput;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.resources.editor.document.IDiagramDocumentProvider#getDiagramDocument(java.lang.
	 * Object)
	 */
	@Override
	public IDiagramDocument getDiagramDocument(Object element) {
		final IDocument doc = getDocument(element);

		if (doc instanceof final IDiagramDocument diagramDocument)
			return diagramDocument;

		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.resources.editor.document.AbstractDocumentProvider#getOperationRunner(org.eclipse.
	 * core.runtime.IProgressMonitor)
	 */
	@Override
	protected IRunnableContext getOperationRunner(IProgressMonitor monitor) {
		return null;
	}

	protected class ResourceSetInfo extends ElementInfo {
		private long myModificationStamp = IResource.NULL_STAMP;
		private WorkspaceSynchronizer mySynchronizer;
		private final Collection<Resource> myUnSynchronizedResources = new ArrayList<>();
		private final IDiagramDocument myDocument;
		private final IEditorInput myEditorInput;
		private boolean myUpdateCache = true;
		private boolean myModifiable;
		private boolean myReadOnly = true;
		private final ResourceSetModificationListener myResourceSetListener;

		/**
		 * @param document
		 * @param editorInput
		 */
		public ResourceSetInfo(IDiagramDocument document, IEditorInput editorInput) {
			super(document);

			myDocument = document;
			myEditorInput = editorInput;
			startResourceListening();
			myResourceSetListener = new ResourceSetModificationListener(this);
			getResourceSet().eAdapters().add(myResourceSetListener);
		}

		/**
		 * @return the modification stamp
		 */
		public long getModificationStamp() {
			return myModificationStamp;
		}

		/**
		 * @param modificationStamp
		 */
		public void setModificationStamp(long modificationStamp) {
			myModificationStamp = modificationStamp;
		}

		/**
		 * @return the editing domain
		 */
		public TransactionalEditingDomain getEditingDomain() {
			return myDocument.getEditingDomain();
		}

		/**
		 * @return the resource set
		 */
		public ResourceSet getResourceSet() {
			return getEditingDomain().getResourceSet();
		}

		/**
		 * @return an iterator over all loaded resources
		 */
		public Iterator<Resource> getLoadedResourcesIterator() {
			return new ArrayList<>(getResourceSet().getResources()).iterator();
		}

		/**
		 * @return the editor input
		 */
		public IEditorInput getEditorInput() {
			return myEditorInput;
		}

		/**
		 * Dispose all loaded resources
		 */
		public void dispose() {
			stopResourceListening();
			getResourceSet().eAdapters().remove(myResourceSetListener);

			for (final Iterator<Resource> it = getLoadedResourcesIterator(); it.hasNext();) {
				final Resource resource = it.next();
				resource.unload();
			}
		}

		/**
		 * @return true if the resource is synchronized
		 */
		public boolean isSynchronized() {
			return myUnSynchronizedResources.isEmpty();
		}

		/**
		 * @param resource
		 */
		public void setUnSynchronized(Resource resource) {
			myUnSynchronizedResources.add(resource);
		}

		/**
		 * @param resource
		 */
		public void setSynchronized(Resource resource) {
			myUnSynchronizedResources.remove(resource);
		}

		/**
		 * Stop listening
		 */
		public final void stopResourceListening() {
			mySynchronizer.dispose();
			mySynchronizer = null;
		}

		/**
		 * Start listening
		 */
		public final void startResourceListening() {
			mySynchronizer = new WorkspaceSynchronizer(getEditingDomain(), new SynchronizerDelegate());
		}

		/**
		 * @return the update cache flag
		 */
		public boolean isUpdateCache() {
			return myUpdateCache;
		}

		/**
		 * @param update
		 */
		public void setUpdateCache(boolean update) {
			myUpdateCache = update;
		}

		/**
		 * @return the modification flag
		 */
		public boolean isModifiable() {
			return myModifiable;
		}

		/**
		 * @param modifiable
		 */
		public void setModifiable(boolean modifiable) {
			myModifiable = modifiable;
		}

		/**
		 * @return the read-only flag
		 */
		public boolean isReadOnly() {
			return myReadOnly;
		}

		/**
		 * @param readOnly
		 */
		public void setReadOnly(boolean readOnly) {
			myReadOnly = readOnly;
		}

		private class SynchronizerDelegate implements WorkspaceSynchronizer.Delegate {
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
				synchronized (ResourceSetInfo.this) {
					if (ResourceSetInfo.this.fCanBeSaved) {
						ResourceSetInfo.this.setUnSynchronized(resource);
						return true;
					}
				}

				Display.getDefault().asyncExec(() -> handleElementChanged(ResourceSetInfo.this, resource, null));

				return true;
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.emf.workspace.util.WorkspaceSynchronizer.Delegate#handleResourceDeleted(org.eclipse.emf.ecore.resource.
			 * Resource)
			 */
			@Override
			public boolean handleResourceDeleted(Resource resource) {
				synchronized (ResourceSetInfo.this) {
					if (ResourceSetInfo.this.fCanBeSaved) {
						ResourceSetInfo.this.setUnSynchronized(resource);
						return true;
					}
				}

				Display.getDefault().asyncExec(() -> fireElementDeleted(ResourceSetInfo.this.getEditorInput()));

				return true;
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.emf.workspace.util.WorkspaceSynchronizer.Delegate#handleResourceMoved(org.eclipse.emf.ecore.resource.
			 * Resource, org.eclipse.emf.common.util.URI)
			 */
			@Override
			public boolean handleResourceMoved(Resource resource, final URI newURI) {
				synchronized (ResourceSetInfo.this) {
					if (ResourceSetInfo.this.fCanBeSaved) {
						ResourceSetInfo.this.setUnSynchronized(resource);
						return true;
					}
				}

				if (myDocument.getDiagram().eResource() == resource)
					Display.getDefault().asyncExec(() -> handleElementMoved(ResourceSetInfo.this.getEditorInput(), newURI));
				else
					handleResourceDeleted(resource);

				return true;
			}
		}
	}

	private class ResourceSetModificationListener extends EContentAdapter {
		private final NotificationFilter myModifiedFilter;
		private final ResourceSetInfo myInfo;

		/**
		 * @param info
		 */
		public ResourceSetModificationListener(ResourceSetInfo info) {
			myInfo = info;
			myModifiedFilter = NotificationFilter.createEventTypeFilter(Notification.SET)
					.or(NotificationFilter.createEventTypeFilter(Notification.UNSET))
					.and(NotificationFilter.createFeatureFilter(Resource.class, Resource.RESOURCE__IS_MODIFIED));
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.emf.ecore.util.EContentAdapter#notifyChanged(org.eclipse.emf.common.notify.Notification)
		 */
		@Override
		public void notifyChanged(Notification notification) {
			if (notification.getNotifier() instanceof ResourceSet)
				super.notifyChanged(notification);

			if (!notification.isTouch() && myModifiedFilter.matches(notification)
					&& notification.getNotifier() instanceof final Resource resource && resource.isLoaded()) {
				boolean modified = false;

				for (final Iterator<Resource> it = myInfo.getLoadedResourcesIterator(); it.hasNext() && !modified;) {
					final Resource nextResource = it.next();

					if (nextResource.isLoaded())
						modified = nextResource.isModified();
				}

				boolean dirtyStateChanged = false;

				synchronized (myInfo) {
					if (modified != myInfo.fCanBeSaved) {
						myInfo.fCanBeSaved = modified;
						dirtyStateChanged = true;
					}

					if (!resource.isModified())
						myInfo.setSynchronized(resource);
				}

				if (dirtyStateChanged) {
					fireElementDirtyStateChanged(myInfo.getEditorInput(), modified);

					if (!modified)
						myInfo.setModificationStamp(computeModificationStamp(myInfo));
				}
			}
		}
	}

}

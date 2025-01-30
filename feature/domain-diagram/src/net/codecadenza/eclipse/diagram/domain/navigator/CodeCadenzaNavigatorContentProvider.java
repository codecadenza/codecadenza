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
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import net.codecadenza.eclipse.diagram.domain.edit.parts.DomainAttributeCompartmentEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.DomainAttributeEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.DomainNamespaceEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.DomainObjectEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.DomainObjectInheritanceEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.EnumAssociationEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.EnumLiteralEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.JavaEnumEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.JavaEnumEnumerationLiteralCompartmentEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.ManyToManyAssociationEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.ManyToOneAssociationEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.OneToManyAssociationEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.OneToOneAssociationEditPart;
import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaVisualIDRegistry;
import net.codecadenza.eclipse.diagram.domain.part.Messages;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.edit.domain.AdapterFactoryEditingDomain;
import org.eclipse.emf.transaction.TransactionalEditingDomain;
import org.eclipse.emf.workspace.util.WorkspaceSynchronizer;
import org.eclipse.gmf.runtime.emf.core.GMFEditingDomainFactory;
import org.eclipse.gmf.runtime.notation.Diagram;
import org.eclipse.gmf.runtime.notation.Edge;
import org.eclipse.gmf.runtime.notation.View;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.navigator.ICommonContentExtensionSite;
import org.eclipse.ui.navigator.ICommonContentProvider;

/**
 * <p>
 * Navigator content provider
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CodeCadenzaNavigatorContentProvider implements ICommonContentProvider {
	private static final Object[] EMPTY_ARRAY = {};
	private Viewer viewer;
	private AdapterFactoryEditingDomain internalEditingDomain;
	private WorkspaceSynchronizer workspaceSynchronizer;
	private Runnable viewerRefresher;

	/**
	 * Constructor
	 */
	public CodeCadenzaNavigatorContentProvider() {
		final TransactionalEditingDomain editingDomain = GMFEditingDomainFactory.INSTANCE.createEditingDomain();

		internalEditingDomain = (AdapterFactoryEditingDomain) editingDomain;

		internalEditingDomain.setResourceToReadOnlyMap(new HashMap<Resource, Boolean>() {
			private static final long serialVersionUID = 362720428985547674L;

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

		viewerRefresher = () -> {
			if (viewer != null)
				viewer.refresh();
		};

		workspaceSynchronizer = new WorkspaceSynchronizer(editingDomain, new WorkspaceSynchronizer.Delegate() {
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
			public void unloadResources() {
				internalEditingDomain.getResourceSet().getResources().forEach(Resource::unload);

				if (viewer != null)
					viewer.getControl().getDisplay().asyncExec(viewerRefresher);
			}
		});
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.viewers.IContentProvider#dispose()
	 */
	@Override
	public void dispose() {
		workspaceSynchronizer.dispose();
		workspaceSynchronizer = null;
		viewerRefresher = null;

		internalEditingDomain.getResourceSet().getResources().forEach(Resource::unload);

		((TransactionalEditingDomain) internalEditingDomain).dispose();
		internalEditingDomain = null;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object,
	 * java.lang.Object)
	 */
	@Override
	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		this.viewer = viewer;
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
			final Resource resource = internalEditingDomain.getResourceSet().getResource(fileURI, true);
			final var result = new ArrayList<>(
					createNavigatorItems(selectViewsByType(resource.getContents(), DomainNamespaceEditPart.MODEL_ID), file, false));

			return result.toArray();
		}

		if (parentElement instanceof final CodeCadenzaNavigatorGroup group)
			return group.getChildren();

		if (parentElement instanceof final CodeCadenzaNavigatorItem navigatorItem) {
			if (navigatorItem.isLeaf() || !isOwnView(navigatorItem.getView()))
				return EMPTY_ARRAY;

			return getViewChildren(navigatorItem.getView(), parentElement);
		}

		/*
		 * Due to plugin.xml restrictions this code will be called only for views representing shortcuts to this diagram elements
		 * created on other diagrams
		 */
		if (parentElement instanceof final IAdaptable adaptable) {
			final View view = adaptable.getAdapter(View.class);

			if (view != null)
				return getViewChildren(view, parentElement);
		}

		return EMPTY_ARRAY;
	}

	/**
	 * @param view
	 * @param parentElement
	 * @return an array of objects
	 */
	private Object[] getViewChildren(View view, Object parentElement) {
		switch (CodeCadenzaVisualIDRegistry.getVisualID(view)) {
			case DomainNamespaceEditPart.VISUAL_ID: {
				return getDomainNamespaceChildren(view, parentElement);
			}
			case DomainObjectEditPart.VISUAL_ID: {
				return getDomainObjectChildren(view, parentElement);
			}
			case JavaEnumEditPart.VISUAL_ID: {
				return getJaveEnumChildren(view, parentElement);
			}
			case OneToOneAssociationEditPart.VISUAL_ID: {
				return getOneToOneAssociationChildren(view, parentElement);
			}
			case ManyToManyAssociationEditPart.VISUAL_ID: {
				return getManyToManyAssociationChildren(view, parentElement);
			}
			case ManyToOneAssociationEditPart.VISUAL_ID: {
				return getManyToOneAssociationChildren(view, parentElement);
			}
			case OneToManyAssociationEditPart.VISUAL_ID: {
				return getOneToManyAssociationChildren(view, parentElement);
			}
			case EnumAssociationEditPart.VISUAL_ID: {
				return getEnumAssociationChildren(view, parentElement);
			}
			case DomainObjectInheritanceEditPart.VISUAL_ID: {
				return getDomainObjectInheritanceChildren(view, parentElement);
			}
		}

		return EMPTY_ARRAY;
	}

	/**
	 * @param view
	 * @param parentElement
	 * @return all children of a domain namespace
	 */
	private Object[] getDomainNamespaceChildren(View view, Object parentElement) {
		final var result = new ArrayList<>();
		result.addAll(getForeignShortcuts((Diagram) view, parentElement));

		final var links = new CodeCadenzaNavigatorGroup(Messages.NavigatorGroupName_DomainNamespace_1000_links,
				"icons/linksNavigatorGroup.gif", parentElement);

		Collection<View> connectedViews = getChildrenByType(Collections.singleton(view), DomainObjectEditPart.VISUAL_ID);
		result.addAll(createNavigatorItems(connectedViews, parentElement, false));
		connectedViews = getChildrenByType(Collections.singleton(view), JavaEnumEditPart.VISUAL_ID);
		result.addAll(createNavigatorItems(connectedViews, parentElement, false));
		connectedViews = getDiagramLinksByType(Collections.singleton(view), OneToOneAssociationEditPart.VISUAL_ID);
		links.addChildren(createNavigatorItems(connectedViews, links, false));
		connectedViews = getDiagramLinksByType(Collections.singleton(view), ManyToManyAssociationEditPart.VISUAL_ID);
		links.addChildren(createNavigatorItems(connectedViews, links, false));
		connectedViews = getDiagramLinksByType(Collections.singleton(view), ManyToOneAssociationEditPart.VISUAL_ID);
		links.addChildren(createNavigatorItems(connectedViews, links, false));
		connectedViews = getDiagramLinksByType(Collections.singleton(view), OneToManyAssociationEditPart.VISUAL_ID);
		links.addChildren(createNavigatorItems(connectedViews, links, false));
		connectedViews = getDiagramLinksByType(Collections.singleton(view), EnumAssociationEditPart.VISUAL_ID);
		links.addChildren(createNavigatorItems(connectedViews, links, false));
		connectedViews = getDiagramLinksByType(Collections.singleton(view), DomainObjectInheritanceEditPart.VISUAL_ID);
		links.addChildren(createNavigatorItems(connectedViews, links, false));

		if (!links.isEmpty())
			result.add(links);

		return result.toArray();
	}

	/**
	 * @param view
	 * @param parentElement
	 * @return all children of a domain object
	 */
	private Object[] getDomainObjectChildren(View view, Object parentElement) {
		final var result = new ArrayList<>();
		final var incominglinks = new CodeCadenzaNavigatorGroup(Messages.NavigatorGroupName_DomainObject_2001_incominglinks,
				"icons/incomingLinksNavigatorGroup.gif", parentElement);
		final var outgoinglinks = new CodeCadenzaNavigatorGroup(Messages.NavigatorGroupName_DomainObject_2001_outgoinglinks,
				"icons/outgoingLinksNavigatorGroup.gif", parentElement);

		Collection<View> connectedViews = getChildrenByType(Collections.singleton(view),
				DomainAttributeCompartmentEditPart.VISUAL_ID);
		connectedViews = getChildrenByType(connectedViews, DomainAttributeEditPart.VISUAL_ID);
		result.addAll(createNavigatorItems(connectedViews, parentElement, false));
		connectedViews = getIncomingLinksByType(Collections.singleton(view), OneToOneAssociationEditPart.VISUAL_ID);
		incominglinks.addChildren(createNavigatorItems(connectedViews, incominglinks, true));
		connectedViews = getOutgoingLinksByType(Collections.singleton(view), OneToOneAssociationEditPart.VISUAL_ID);
		outgoinglinks.addChildren(createNavigatorItems(connectedViews, outgoinglinks, true));
		connectedViews = getIncomingLinksByType(Collections.singleton(view), ManyToManyAssociationEditPart.VISUAL_ID);
		incominglinks.addChildren(createNavigatorItems(connectedViews, incominglinks, true));
		connectedViews = getOutgoingLinksByType(Collections.singleton(view), ManyToManyAssociationEditPart.VISUAL_ID);
		outgoinglinks.addChildren(createNavigatorItems(connectedViews, outgoinglinks, true));
		connectedViews = getIncomingLinksByType(Collections.singleton(view), ManyToOneAssociationEditPart.VISUAL_ID);
		incominglinks.addChildren(createNavigatorItems(connectedViews, incominglinks, true));
		connectedViews = getOutgoingLinksByType(Collections.singleton(view), ManyToOneAssociationEditPart.VISUAL_ID);
		outgoinglinks.addChildren(createNavigatorItems(connectedViews, outgoinglinks, true));
		connectedViews = getIncomingLinksByType(Collections.singleton(view), OneToManyAssociationEditPart.VISUAL_ID);
		incominglinks.addChildren(createNavigatorItems(connectedViews, incominglinks, true));
		connectedViews = getOutgoingLinksByType(Collections.singleton(view), OneToManyAssociationEditPart.VISUAL_ID);
		outgoinglinks.addChildren(createNavigatorItems(connectedViews, outgoinglinks, true));
		connectedViews = getOutgoingLinksByType(Collections.singleton(view), EnumAssociationEditPart.VISUAL_ID);
		outgoinglinks.addChildren(createNavigatorItems(connectedViews, outgoinglinks, true));
		connectedViews = getIncomingLinksByType(Collections.singleton(view), DomainObjectInheritanceEditPart.VISUAL_ID);
		incominglinks.addChildren(createNavigatorItems(connectedViews, incominglinks, true));
		connectedViews = getOutgoingLinksByType(Collections.singleton(view), DomainObjectInheritanceEditPart.VISUAL_ID);
		outgoinglinks.addChildren(createNavigatorItems(connectedViews, outgoinglinks, true));

		if (!incominglinks.isEmpty())
			result.add(incominglinks);

		if (!outgoinglinks.isEmpty())
			result.add(outgoinglinks);

		return result.toArray();
	}

	/**
	 * @param view
	 * @param parentElement
	 * @return all children of a domain object inheritance
	 */
	private Object[] getDomainObjectInheritanceChildren(View view, Object parentElement) {
		final var result = new ArrayList<>();
		final var target = new CodeCadenzaNavigatorGroup(Messages.NavigatorGroupName_DomainInheritance_4005_target,
				"icons/linkTargetNavigatorGroup.gif", parentElement);
		final var source = new CodeCadenzaNavigatorGroup(Messages.NavigatorGroupName_DomainInheritance_4005_source,
				"icons/linkSourceNavigatorGroup.gif", parentElement);

		Collection<View> connectedViews = getLinksTargetByType(Collections.singleton(view), DomainObjectEditPart.VISUAL_ID);
		target.addChildren(createNavigatorItems(connectedViews, target, true));
		connectedViews = getLinksSourceByType(Collections.singleton(view), DomainObjectEditPart.VISUAL_ID);
		source.addChildren(createNavigatorItems(connectedViews, source, true));

		if (!target.isEmpty())
			result.add(target);

		if (!source.isEmpty())
			result.add(source);

		return result.toArray();
	}

	/**
	 * @param view
	 * @param parentElement
	 * @return all children of a one-to-many association
	 */
	private Object[] getOneToManyAssociationChildren(View view, Object parentElement) {
		final var result = new ArrayList<>();
		final var target = new CodeCadenzaNavigatorGroup(Messages.NavigatorGroupName_OneToManyAssociation_4006_target,
				"icons/linkTargetNavigatorGroup.gif", parentElement);
		final var source = new CodeCadenzaNavigatorGroup(Messages.NavigatorGroupName_OneToManyAssociation_4006_source,
				"icons/linkSourceNavigatorGroup.gif", parentElement);

		Collection<View> connectedViews = getLinksTargetByType(Collections.singleton(view), DomainObjectEditPart.VISUAL_ID);
		target.addChildren(createNavigatorItems(connectedViews, target, true));
		connectedViews = getLinksSourceByType(Collections.singleton(view), DomainObjectEditPart.VISUAL_ID);
		source.addChildren(createNavigatorItems(connectedViews, source, true));

		if (!target.isEmpty())
			result.add(target);

		if (!source.isEmpty())
			result.add(source);

		return result.toArray();
	}

	/**
	 * @param view
	 * @param parentElement
	 * @return all children of a many-to-one association
	 */
	private Object[] getManyToOneAssociationChildren(View view, Object parentElement) {
		final var result = new ArrayList<>();
		final var target = new CodeCadenzaNavigatorGroup(Messages.NavigatorGroupName_ManyToOneAssociation_4004_target,
				"icons/linkTargetNavigatorGroup.gif", parentElement);
		final var source = new CodeCadenzaNavigatorGroup(Messages.NavigatorGroupName_ManyToOneAssociation_4004_source,
				"icons/linkSourceNavigatorGroup.gif", parentElement);

		Collection<View> connectedViews = getLinksTargetByType(Collections.singleton(view), DomainObjectEditPart.VISUAL_ID);
		target.addChildren(createNavigatorItems(connectedViews, target, true));
		connectedViews = getLinksSourceByType(Collections.singleton(view), DomainObjectEditPart.VISUAL_ID);
		source.addChildren(createNavigatorItems(connectedViews, source, true));

		if (!target.isEmpty())
			result.add(target);

		if (!source.isEmpty())
			result.add(source);

		return result.toArray();
	}

	/**
	 * @param view
	 * @param parentElement
	 * @return all children of a many-to-many association
	 */
	private Object[] getManyToManyAssociationChildren(View view, Object parentElement) {
		final var result = new ArrayList<>();
		final var target = new CodeCadenzaNavigatorGroup(Messages.NavigatorGroupName_ManyToManyAssociation_4002_target,
				"icons/linkTargetNavigatorGroup.gif", parentElement);
		final var source = new CodeCadenzaNavigatorGroup(Messages.NavigatorGroupName_ManyToManyAssociation_4002_source,
				"icons/linkSourceNavigatorGroup.gif", parentElement);

		Collection<View> connectedViews = getLinksTargetByType(Collections.singleton(view), DomainObjectEditPart.VISUAL_ID);
		target.addChildren(createNavigatorItems(connectedViews, target, true));
		connectedViews = getLinksSourceByType(Collections.singleton(view), DomainObjectEditPart.VISUAL_ID);
		source.addChildren(createNavigatorItems(connectedViews, source, true));

		if (!target.isEmpty())
			result.add(target);

		if (!source.isEmpty())
			result.add(source);

		return result.toArray();
	}

	/**
	 * @param view
	 * @param parentElement
	 * @return all children of a one-to-one association
	 */
	private Object[] getOneToOneAssociationChildren(View view, Object parentElement) {
		final var result = new ArrayList<>();
		final var target = new CodeCadenzaNavigatorGroup(Messages.NavigatorGroupName_OneToOneAssociation_4003_target,
				"icons/linkTargetNavigatorGroup.gif", parentElement);
		final var source = new CodeCadenzaNavigatorGroup(Messages.NavigatorGroupName_OneToOneAssociation_4003_source,
				"icons/linkSourceNavigatorGroup.gif", parentElement);

		Collection<View> connectedViews = getLinksTargetByType(Collections.singleton(view), DomainObjectEditPart.VISUAL_ID);
		target.addChildren(createNavigatorItems(connectedViews, target, true));
		connectedViews = getLinksSourceByType(Collections.singleton(view), DomainObjectEditPart.VISUAL_ID);
		source.addChildren(createNavigatorItems(connectedViews, source, true));

		if (!target.isEmpty())
			result.add(target);

		if (!source.isEmpty())
			result.add(source);

		return result.toArray();
	}

	/**
	 * @param view
	 * @param parentElement
	 * @return all children of a Java enum
	 */
	private Object[] getJaveEnumChildren(View view, Object parentElement) {
		final var result = new ArrayList<>();
		final var incominglinks = new CodeCadenzaNavigatorGroup(Messages.NavigatorGroupName_JavaEnum_2002_incominglinks,
				"icons/incomingLinksNavigatorGroup.gif", parentElement);

		Collection<View> connectedViews = getChildrenByType(Collections.singleton(view),
				JavaEnumEnumerationLiteralCompartmentEditPart.VISUAL_ID);
		connectedViews = getChildrenByType(connectedViews, EnumLiteralEditPart.VISUAL_ID);
		result.addAll(createNavigatorItems(connectedViews, parentElement, false));
		connectedViews = getIncomingLinksByType(Collections.singleton(view), EnumAssociationEditPart.VISUAL_ID);
		incominglinks.addChildren(createNavigatorItems(connectedViews, incominglinks, true));

		if (!incominglinks.isEmpty())
			result.add(incominglinks);

		return result.toArray();
	}

	/**
	 * @param view
	 * @param parentElement
	 * @return all children of an enum association
	 */
	private Object[] getEnumAssociationChildren(View view, Object parentElement) {
		final var result = new ArrayList<>();
		final var target = new CodeCadenzaNavigatorGroup(Messages.NavigatorGroupName_EnumAssociation_4001_target,
				"icons/linkTargetNavigatorGroup.gif", parentElement);
		final var source = new CodeCadenzaNavigatorGroup(Messages.NavigatorGroupName_EnumAssociation_4001_source,
				"icons/linkSourceNavigatorGroup.gif", parentElement);

		Collection<View> connectedViews = getLinksTargetByType(Collections.singleton(view), JavaEnumEditPart.VISUAL_ID);
		target.addChildren(createNavigatorItems(connectedViews, target, true));
		connectedViews = getLinksSourceByType(Collections.singleton(view), DomainObjectEditPart.VISUAL_ID);
		source.addChildren(createNavigatorItems(connectedViews, source, true));

		if (!target.isEmpty())
			result.add(target);

		if (!source.isEmpty())
			result.add(source);

		return result.toArray();
	}

	/**
	 * @param edges
	 * @param visualID
	 * @return a link collection
	 */
	private Collection<View> getLinksSourceByType(Collection<View> edges, int visualID) {
		final var result = new ArrayList<View>();
		final String type = CodeCadenzaVisualIDRegistry.getType(visualID);

		edges.stream().map(Edge.class::cast).forEach(nextEdge -> {
			final View nextEdgeSource = nextEdge.getSource();

			if (type.equals(nextEdgeSource.getType()) && isOwnView(nextEdgeSource))
				result.add(nextEdgeSource);
		});

		return result;
	}

	/**
	 * @param edges
	 * @param visualID
	 * @return a collection of target links
	 */
	private Collection<View> getLinksTargetByType(Collection<View> edges, int visualID) {
		final var result = new ArrayList<View>();
		final String type = CodeCadenzaVisualIDRegistry.getType(visualID);

		edges.stream().map(Edge.class::cast).forEach(nextEdge -> {
			final View nextEdgeTarget = nextEdge.getTarget();

			if (type.equals(nextEdgeTarget.getType()) && isOwnView(nextEdgeTarget))
				result.add(nextEdgeTarget);
		});

		return result;
	}

	/**
	 * @param nodes
	 * @param visualID
	 * @return a collection of outgoing links
	 */
	@SuppressWarnings("unchecked")
	private Collection<View> getOutgoingLinksByType(Collection<View> nodes, int visualID) {
		final var result = new ArrayList<View>();
		final String type = CodeCadenzaVisualIDRegistry.getType(visualID);

		nodes.forEach(node -> result.addAll(selectViewsByType(node.getSourceEdges(), type)));

		return result;
	}

	/**
	 * @param nodes
	 * @param visualID
	 * @return a collection of incoming links
	 */
	@SuppressWarnings("unchecked")
	private Collection<View> getIncomingLinksByType(Collection<View> nodes, int visualID) {
		final var result = new ArrayList<View>();
		final String type = CodeCadenzaVisualIDRegistry.getType(visualID);

		nodes.forEach(node -> result.addAll(selectViewsByType(node.getTargetEdges(), type)));

		return result;
	}

	/**
	 * @param nodes
	 * @param visualID
	 * @return a collection of children
	 */
	@SuppressWarnings("unchecked")
	private Collection<View> getChildrenByType(Collection<View> nodes, int visualID) {
		final var result = new ArrayList<View>();
		final String type = CodeCadenzaVisualIDRegistry.getType(visualID);

		nodes.forEach(node -> result.addAll(selectViewsByType(node.getChildren(), type)));

		return result;
	}

	/**
	 * @param diagrams
	 * @param visualID
	 * @return a collection of diagram links
	 */
	@SuppressWarnings("unchecked")
	private Collection<View> getDiagramLinksByType(Collection<?> diagrams, int visualID) {
		final var result = new ArrayList<View>();
		final String type = CodeCadenzaVisualIDRegistry.getType(visualID);

		for (final Object name : diagrams) {
			final var nextDiagram = (Diagram) name;
			result.addAll(selectViewsByType(nextDiagram.getEdges(), type));
		}

		return result;
	}

	/**
	 * @param views
	 * @param type
	 * @return a collection of selected views
	 */
	private Collection<View> selectViewsByType(Collection<EObject> views, String type) {
		final var result = new ArrayList<View>();

		views.stream().map(View.class::cast).forEach(nextView -> {
			if (type.equals(nextView.getType()) && isOwnView(nextView))
				result.add(nextView);
		});

		return result;
	}

	/**
	 * @param view
	 * @return true if the given view belongs to this plug-in
	 */
	private boolean isOwnView(View view) {
		return DomainNamespaceEditPart.MODEL_ID.equals(CodeCadenzaVisualIDRegistry.getModelID(view));
	}

	/**
	 * @param views
	 * @param parent
	 * @param isLeafs
	 * @return a collection of navigator items
	 */
	private Collection<CodeCadenzaNavigatorItem> createNavigatorItems(Collection<View> views, Object parent, boolean isLeafs) {
		return views.stream().map(view -> new CodeCadenzaNavigatorItem(view, parent, isLeafs)).toList();
	}

	/**
	 * @param diagram
	 * @param parent
	 * @return a collection of shortcuts
	 */
	private Collection<CodeCadenzaNavigatorItem> getForeignShortcuts(Diagram diagram, Object parent) {
		final var result = new ArrayList<View>();

		for (final Iterator<View> it = diagram.getChildren().iterator(); it.hasNext();) {
			final View nextView = it.next();

			if (!isOwnView(nextView) && nextView.getEAnnotation("Shortcut") != null)
				result.add(nextView);
		}

		return createNavigatorItems(result, parent, false);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.viewers.ITreeContentProvider#getParent(java.lang.Object)
	 */
	@Override
	public Object getParent(Object element) {
		if (element instanceof final CodeCadenzaAbstractNavigatorItem abstractNavigatorItem)
			return abstractNavigatorItem.getParent();

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

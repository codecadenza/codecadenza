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
package net.codecadenza.eclipse.diagram.domain.edit.policies;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import net.codecadenza.eclipse.diagram.domain.edit.parts.DomainAttributeEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.DomainNamespaceEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.DomainObjectEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.DomainObjectInheritanceEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.EnumAssociationEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.EnumLiteralEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.JavaEnumEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.ManyToManyAssociationEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.ManyToOneAssociationEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.OneToManyAssociationEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.OneToOneAssociationEditPart;
import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaDiagramUpdater;
import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaLinkDescriptor;
import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaNodeDescriptor;
import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaVisualIDRegistry;
import net.codecadenza.eclipse.model.domain.DomainPackage;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.gef.EditPart;
import org.eclipse.gef.commands.Command;
import org.eclipse.gmf.runtime.diagram.core.util.ViewUtil;
import org.eclipse.gmf.runtime.diagram.ui.commands.DeferredLayoutCommand;
import org.eclipse.gmf.runtime.diagram.ui.commands.ICommandProxy;
import org.eclipse.gmf.runtime.diagram.ui.editparts.IGraphicalEditPart;
import org.eclipse.gmf.runtime.diagram.ui.editpolicies.CanonicalConnectionEditPolicy;
import org.eclipse.gmf.runtime.diagram.ui.requests.CreateConnectionViewRequest;
import org.eclipse.gmf.runtime.diagram.ui.requests.RequestConstants;
import org.eclipse.gmf.runtime.notation.Diagram;
import org.eclipse.gmf.runtime.notation.Edge;
import org.eclipse.gmf.runtime.notation.View;

/**
 * <p>
 * Policy class
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DomainNamespaceCanonicalEditPolicy extends CanonicalConnectionEditPolicy {
	private Set<EStructuralFeature> featuresToSynchronize;

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.editpolicies.CanonicalConnectionEditPolicy#getSemanticChildrenList()
	 */
	@Override
	protected List<EObject> getSemanticChildrenList() {
		final var viewObject = (View) getHost().getModel();
		final var result = new LinkedList<EObject>();

		for (final CodeCadenzaNodeDescriptor nodeDescriptor : CodeCadenzaDiagramUpdater.getDomainNamespace_1000SemanticChildren(viewObject))
			result.add(nodeDescriptor.getModelElement());

		return result;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.editpolicies.CanonicalConnectionEditPolicy#shouldDeleteView(org.eclipse.gmf.runtime.
	 * notation.View)
	 */
	@Override
	protected boolean shouldDeleteView(View view) {
		return true;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.editpolicies.CanonicalConnectionEditPolicy#isOrphaned(java.util.Collection,
	 * org.eclipse.gmf.runtime.notation.View)
	 */
	@Override
	protected boolean isOrphaned(Collection<EObject> semanticChildren, final View view) {
		if (view.getEAnnotation("Shortcut") != null)
			return CodeCadenzaDiagramUpdater.isShortcutOrphaned(view);

		final int visualID = CodeCadenzaVisualIDRegistry.getVisualID(view);

		switch (visualID) {
			case DomainObjectEditPart.VISUAL_ID, JavaEnumEditPart.VISUAL_ID:
				if (!semanticChildren.contains(view.getElement()))
					return true;
		}

		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.editpolicies.CanonicalConnectionEditPolicy#getDefaultFactoryHint()
	 */
	@Override
	protected String getDefaultFactoryHint() {
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.editpolicies.CanonicalEditPolicy#getFeaturesToSynchronize()
	 */
	@Override
	protected Set<EStructuralFeature> getFeaturesToSynchronize() {
		if (featuresToSynchronize == null) {
			featuresToSynchronize = new HashSet<>();
			featuresToSynchronize.add(DomainPackage.eINSTANCE.getDomainNamespace_DomainObjects());
			featuresToSynchronize.add(DomainPackage.eINSTANCE.getDomainNamespace_Enumerations());
		}

		return featuresToSynchronize;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.editpolicies.CanonicalConnectionEditPolicy#getSemanticConnectionsList()
	 */
	@Override
	protected List<EObject> getSemanticConnectionsList() {
		return Collections.emptyList();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.editpolicies.CanonicalConnectionEditPolicy#getSourceElement(org.eclipse.emf.ecore.
	 * EObject)
	 */
	@Override
	protected EObject getSourceElement(EObject relationship) {
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.editpolicies.CanonicalConnectionEditPolicy#getTargetElement(org.eclipse.emf.ecore.
	 * EObject)
	 */
	@Override
	protected EObject getTargetElement(EObject relationship) {
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.editpolicies.CanonicalConnectionEditPolicy#shouldIncludeConnection(org.eclipse.gmf.
	 * runtime.notation.Edge, java.util.Collection)
	 */
	@Override
	protected boolean shouldIncludeConnection(Edge connector, Collection<View> children) {
		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.ui.editpolicies.CanonicalConnectionEditPolicy#refreshSemantic()
	 */
	@Override
	protected void refreshSemantic() {
		final var createdViews = new LinkedList<>(refreshSemanticChildren());

		final var createdConnectionViews = new LinkedList<>(refreshSemanticConnections());
		createdConnectionViews.addAll(refreshConnections());

		if (createdViews.size() > 1) {
			// Refresh the container layout
			final var layoutCmd = new DeferredLayoutCommand(host().getEditingDomain(), createdViews, host());
			executeCommand(new ICommandProxy(layoutCmd));
		}

		createdViews.addAll(createdConnectionViews);
		makeViewsImmutable(createdViews);
	}

	/**
	 * @return the diagram
	 */
	private Diagram getDiagram() {
		return ((View) getHost().getModel()).getDiagram();
	}

	/**
	 * @return the connections
	 */
	public Collection<IAdaptable> refreshConnections() {
		final var domain2NotationMap = new HashMap<EObject, View>();
		final Collection<CodeCadenzaLinkDescriptor> linkDescriptors = collectAllLinks(getDiagram(), domain2NotationMap);
		final var existingLinks = new LinkedList<View>(getDiagram().getEdges());

		for (final Iterator<View> linksIterator = existingLinks.iterator(); linksIterator.hasNext();) {
			final var nextDiagramLink = (Edge) linksIterator.next();
			final int diagramLinkVisualID = CodeCadenzaVisualIDRegistry.getVisualID(nextDiagramLink);

			if (diagramLinkVisualID == -1) {
				if (nextDiagramLink.getSource() != null && nextDiagramLink.getTarget() != null)
					linksIterator.remove();

				continue;
			}

			final EObject diagramLinkObject = nextDiagramLink.getElement();
			final EObject diagramLinkSrc = nextDiagramLink.getSource().getElement();
			final EObject diagramLinkDst = nextDiagramLink.getTarget().getElement();

			for (final Iterator<CodeCadenzaLinkDescriptor> LinkDescriptorsIterator = linkDescriptors.iterator(); LinkDescriptorsIterator
					.hasNext();) {
				final CodeCadenzaLinkDescriptor nextLinkDescriptor = LinkDescriptorsIterator.next();

				if (diagramLinkObject == nextLinkDescriptor.getModelElement() && diagramLinkSrc == nextLinkDescriptor.getSource()
						&& diagramLinkDst == nextLinkDescriptor.getDestination() && diagramLinkVisualID == nextLinkDescriptor.getVisualID()) {
					linksIterator.remove();
					LinkDescriptorsIterator.remove();
				}
			}
		}

		deleteViews(existingLinks.iterator());

		return createConnections(linkDescriptors, domain2NotationMap);
	}

	/**
	 * @param view
	 * @param domain2NotationMap
	 * @return the links
	 */
	private Collection<CodeCadenzaLinkDescriptor> collectAllLinks(View view, Map<EObject, View> domain2NotationMap) {
		if (!DomainNamespaceEditPart.MODEL_ID.equals(CodeCadenzaVisualIDRegistry.getModelID(view)))
			return Collections.emptyList();

		final var result = new LinkedList<CodeCadenzaLinkDescriptor>();

		switch (CodeCadenzaVisualIDRegistry.getVisualID(view)) {
			case DomainNamespaceEditPart.VISUAL_ID: {
				result.addAll(getLinks(view, domain2NotationMap, CodeCadenzaDiagramUpdater::getDomainNamespace_1000ContainedLinks));
				break;
			}
			case DomainObjectEditPart.VISUAL_ID: {
				result.addAll(getLinks(view, domain2NotationMap, CodeCadenzaDiagramUpdater::getDomaiObject_2001ContainedLinks));
				break;
			}
			case JavaEnumEditPart.VISUAL_ID: {
				result.addAll(getLinks(view, domain2NotationMap, CodeCadenzaDiagramUpdater::getJavaEnum_2002ContainedLinks));
				break;
			}
			case DomainAttributeEditPart.VISUAL_ID: {
				result.addAll(getLinks(view, domain2NotationMap, CodeCadenzaDiagramUpdater::getDomainAttribute_3001ContainedLinks));
				break;
			}
			case EnumLiteralEditPart.VISUAL_ID: {
				result.addAll(getLinks(view, domain2NotationMap, CodeCadenzaDiagramUpdater::getEnumLiteral_3002ContainedLinks));
				break;
			}
			case OneToOneAssociationEditPart.VISUAL_ID: {
				result.addAll(getLinks(view, domain2NotationMap, CodeCadenzaDiagramUpdater::getOneToOneAssociation_4003ContainedLinks));
				break;
			}
			case ManyToManyAssociationEditPart.VISUAL_ID: {
				result.addAll(getLinks(view, domain2NotationMap, CodeCadenzaDiagramUpdater::getManyToManyAssociation_4002ContainedLinks));
				break;
			}
			case ManyToOneAssociationEditPart.VISUAL_ID: {
				result.addAll(getLinks(view, domain2NotationMap, CodeCadenzaDiagramUpdater::getManyToOneAssociation_4004ContainedLinks));
				break;
			}
			case OneToManyAssociationEditPart.VISUAL_ID: {
				result.addAll(getLinks(view, domain2NotationMap, CodeCadenzaDiagramUpdater::getOneToManyAssociation_4006ContainedLinks));
				break;
			}
			case EnumAssociationEditPart.VISUAL_ID: {
				result.addAll(getLinks(view, domain2NotationMap, CodeCadenzaDiagramUpdater::getEnumAssociation_4001ContainedLinks));
				break;
			}
			case DomainObjectInheritanceEditPart.VISUAL_ID: {
				result.addAll(getLinks(view, domain2NotationMap, CodeCadenzaDiagramUpdater::getDomainInheritance_4005ContainedLinks));
				break;
			}
		}

		for (final Iterator<View> children = view.getChildren().iterator(); children.hasNext();)
			result.addAll(collectAllLinks(children.next(), domain2NotationMap));

		for (final Iterator<View> edges = view.getSourceEdges().iterator(); edges.hasNext();)
			result.addAll(collectAllLinks(edges.next(), domain2NotationMap));

		return result;
	}

	/**
	 * Get all links from the provided link descriptor function
	 * @param view
	 * @param domain2NotationMap
	 * @param linkDescriptorFunction
	 * @return a collection that contains all links
	 */
	private Collection<CodeCadenzaLinkDescriptor> getLinks(View view, Map<EObject, View> domain2NotationMap,
			Function<View, List<CodeCadenzaLinkDescriptor>> linkDescriptorFunction) {
		final var result = new ArrayList<CodeCadenzaLinkDescriptor>();

		if (!domain2NotationMap.containsKey(view.getElement()))
			result.addAll(linkDescriptorFunction.apply(view));

		if (!domain2NotationMap.containsKey(view.getElement()) || view.getEAnnotation("Shortcut") == null)
			domain2NotationMap.put(view.getElement(), view);

		return result;
	}

	/**
	 * @param linkDescriptors
	 * @param domain2NotationMap
	 * @return the connections
	 */
	private Collection<IAdaptable> createConnections(Collection<CodeCadenzaLinkDescriptor> linkDescriptors,
			Map<EObject, View> domain2NotationMap) {
		final var adapters = new LinkedList<IAdaptable>();

		for (final CodeCadenzaLinkDescriptor linkDescriptor : linkDescriptors) {
			final EditPart sourceEditPart = getEditPart(linkDescriptor.getSource(), domain2NotationMap);
			final EditPart targetEditPart = getEditPart(linkDescriptor.getDestination(), domain2NotationMap);

			if (sourceEditPart == null || targetEditPart == null)
				continue;

			final var descriptor = new CreateConnectionViewRequest.ConnectionViewDescriptor(linkDescriptor.getSemanticAdapter(), null,
					ViewUtil.APPEND, false, ((IGraphicalEditPart) getHost()).getDiagramPreferencesHint());

			final var ccr = new CreateConnectionViewRequest(descriptor);
			ccr.setType(RequestConstants.REQ_CONNECTION_START);
			ccr.setSourceEditPart(sourceEditPart);

			sourceEditPart.getCommand(ccr);

			ccr.setTargetEditPart(targetEditPart);
			ccr.setType(RequestConstants.REQ_CONNECTION_END);

			final Command cmd = targetEditPart.getCommand(ccr);

			if (cmd != null && cmd.canExecute()) {
				executeCommand(cmd);
				final var viewAdapter = (IAdaptable) ccr.getNewObject();

				if (viewAdapter != null)
					adapters.add(viewAdapter);
			}
		}

		return adapters;
	}

	/**
	 * @param domainModelElement
	 * @param domain2NotationMap
	 * @return the edit part
	 */
	private EditPart getEditPart(EObject domainModelElement, Map<EObject, View> domain2NotationMap) {
		final View view = domain2NotationMap.get(domainModelElement);

		if (view != null)
			return getHost().getViewer().getEditPartRegistry().get(view);

		return null;
	}

}

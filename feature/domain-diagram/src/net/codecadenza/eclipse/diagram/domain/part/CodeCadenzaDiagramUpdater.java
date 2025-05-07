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

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
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
import net.codecadenza.eclipse.diagram.domain.providers.CodeCadenzaElementTypes;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainInheritance;
import net.codecadenza.eclipse.model.domain.DomainNamespace;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.DomainPackage;
import net.codecadenza.eclipse.model.domain.EnumAssociation;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.domain.OneToOneAssociation;
import net.codecadenza.eclipse.model.java.EnumLiteral;
import net.codecadenza.eclipse.model.java.JavaEnum;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.gmf.runtime.notation.View;

/**
 * <p>
 * Diagram updater
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CodeCadenzaDiagramUpdater {
	/**
	 * Prevent instantiation
	 */
	private CodeCadenzaDiagramUpdater() {

	}

	/**
	 * @param view
	 * @return true if the shortcut is orphaned
	 */
	public static boolean isShortcutOrphaned(View view) {
		return !view.isSetElement() || view.getElement() == null || view.getElement().eIsProxy();
	}

	/**
	 * @param view
	 * @return a list containing the semantic children
	 */
	public static List<CodeCadenzaNodeDescriptor> getSemanticChildren(View view) {
		switch (CodeCadenzaVisualIDRegistry.getVisualID(view)) {
			case DomainAttributeCompartmentEditPart.VISUAL_ID:
				return getDomainObjectAttributeCompartment_7001SemanticChildren(view);
			case JavaEnumEnumerationLiteralCompartmentEditPart.VISUAL_ID:
				return getJavaEnumEnumerationLiteralCompartment_7002SemanticChildren(view);
			case DomainNamespaceEditPart.VISUAL_ID:
				return getDomainNamespace_1000SemanticChildren(view);
		}

		return Collections.emptyList();
	}

	/**
	 * @param view
	 * @return a list containing the domain object attribute compartments
	 */
	public static List<CodeCadenzaNodeDescriptor> getDomainObjectAttributeCompartment_7001SemanticChildren(View view) {
		if (!(view.eContainer() instanceof final View containerView))
			return Collections.emptyList();

		if (!containerView.isSetElement())
			return Collections.emptyList();

		final var modelElement = (DomainObject) containerView.getElement();
		final var result = new LinkedList<CodeCadenzaNodeDescriptor>();

		for (final DomainAttribute childElement : modelElement.getAttributes()) {
			final int visualID = CodeCadenzaVisualIDRegistry.getNodeVisualID(view, childElement);

			if (visualID == DomainAttributeEditPart.VISUAL_ID)
				result.add(new CodeCadenzaNodeDescriptor(childElement, visualID));
		}

		return result;
	}

	/**
	 * @param view
	 * @return a list containing the Java enumeration literal compartments
	 */
	public static List<CodeCadenzaNodeDescriptor> getJavaEnumEnumerationLiteralCompartment_7002SemanticChildren(View view) {
		if (!(view.eContainer() instanceof final View containerView))
			return Collections.emptyList();

		if (!containerView.isSetElement())
			return Collections.emptyList();

		final var modelElement = (JavaEnum) containerView.getElement();
		final var result = new LinkedList<CodeCadenzaNodeDescriptor>();

		for (final EnumLiteral childElement : modelElement.getEnumerationValues()) {
			final int visualID = CodeCadenzaVisualIDRegistry.getNodeVisualID(view, childElement);

			if (visualID == EnumLiteralEditPart.VISUAL_ID)
				result.add(new CodeCadenzaNodeDescriptor(childElement, visualID));
		}

		return result;
	}

	/**
	 * @param view
	 * @return a list containing the domain namespace semantic children
	 */
	public static List<CodeCadenzaNodeDescriptor> getDomainNamespace_1000SemanticChildren(View view) {
		if (!view.isSetElement())
			return Collections.emptyList();

		final var modelElement = (DomainNamespace) view.getElement();
		final var result = new LinkedList<CodeCadenzaNodeDescriptor>();

		for (final DomainObject childElement : modelElement.getDomainObjects()) {
			final int visualID = CodeCadenzaVisualIDRegistry.getNodeVisualID(view, childElement);

			if (visualID == DomainObjectEditPart.VISUAL_ID)
				result.add(new CodeCadenzaNodeDescriptor(childElement, visualID));
		}

		for (final JavaEnum childElement : modelElement.getEnumerations()) {
			final int visualID = CodeCadenzaVisualIDRegistry.getNodeVisualID(view, childElement);

			if (visualID == JavaEnumEditPart.VISUAL_ID)
				result.add(new CodeCadenzaNodeDescriptor(childElement, visualID));
		}

		return result;
	}

	/**
	 * @param view
	 * @return a list of links
	 */
	public static List<CodeCadenzaLinkDescriptor> getContainedLinks(View view) {
		switch (CodeCadenzaVisualIDRegistry.getVisualID(view)) {
			case DomainNamespaceEditPart.VISUAL_ID:
				return getDomainNamespace_1000ContainedLinks(view);
			case DomainObjectEditPart.VISUAL_ID:
				return getDomaiObject_2001ContainedLinks(view);
			case JavaEnumEditPart.VISUAL_ID:
				return getJavaEnum_2002ContainedLinks(view);
			case DomainAttributeEditPart.VISUAL_ID:
				return getDomainAttribute_3001ContainedLinks(view);
			case EnumLiteralEditPart.VISUAL_ID:
				return getEnumLiteral_3002ContainedLinks(view);
			case OneToOneAssociationEditPart.VISUAL_ID:
				return getOneToOneAssociation_4003ContainedLinks(view);
			case ManyToManyAssociationEditPart.VISUAL_ID:
				return getManyToManyAssociation_4002ContainedLinks(view);
			case ManyToOneAssociationEditPart.VISUAL_ID:
				return getManyToOneAssociation_4004ContainedLinks(view);
			case OneToManyAssociationEditPart.VISUAL_ID:
				return getOneToManyAssociation_4006ContainedLinks(view);
			case EnumAssociationEditPart.VISUAL_ID:
				return getEnumAssociation_4001ContainedLinks(view);
			case DomainObjectInheritanceEditPart.VISUAL_ID:
				return getDomainInheritance_4005ContainedLinks(view);
		}

		return Collections.emptyList();
	}

	/**
	 * @param view
	 * @return a list of incoming links
	 */
	public static List<CodeCadenzaLinkDescriptor> getIncomingLinks(View view) {
		switch (CodeCadenzaVisualIDRegistry.getVisualID(view)) {
			case DomainObjectEditPart.VISUAL_ID:
				return getDomainObject_2001IncomingLinks(view);
			case JavaEnumEditPart.VISUAL_ID:
				return getJavaEnum_2002IncomingLinks(view);
			case DomainAttributeEditPart.VISUAL_ID:
				return getDomainAttribute_3001IncomingLinks(view);
			case EnumLiteralEditPart.VISUAL_ID:
				return getEnumLiteral_3002IncomingLinks(view);
			case OneToOneAssociationEditPart.VISUAL_ID:
				return getOneToOneAssociation_4003IncomingLinks(view);
			case ManyToManyAssociationEditPart.VISUAL_ID:
				return getManyToManyAssociation_4002IncomingLinks(view);
			case ManyToOneAssociationEditPart.VISUAL_ID:
				return getManyToOneAssociation_4004IncomingLinks(view);
			case OneToManyAssociationEditPart.VISUAL_ID:
				return getOneToManyAssociation_4006IncomingLinks(view);
			case EnumAssociationEditPart.VISUAL_ID:
				return getEnumAssociation_4001IncomingLinks(view);
			case DomainObjectInheritanceEditPart.VISUAL_ID:
				return getDomainObjectInheritance_4005IncomingLinks(view);
		}

		return Collections.emptyList();
	}

	/**
	 * @param view
	 * @return a list containing outgoing links
	 */
	public static List<CodeCadenzaLinkDescriptor> getOutgoingLinks(View view) {
		switch (CodeCadenzaVisualIDRegistry.getVisualID(view)) {
			case DomainObjectEditPart.VISUAL_ID:
				return getDomainObject_2001OutgoingLinks(view);
			case JavaEnumEditPart.VISUAL_ID:
				return getJavaEnum_2002OutgoingLinks(view);
			case DomainAttributeEditPart.VISUAL_ID:
				return getDomainAttribute_3001OutgoingLinks(view);
			case EnumLiteralEditPart.VISUAL_ID:
				return getEnumLiteral_3002OutgoingLinks(view);
			case OneToOneAssociationEditPart.VISUAL_ID:
				return getOneToOneAssociation_4003OutgoingLinks(view);
			case ManyToManyAssociationEditPart.VISUAL_ID:
				return getManyToManyAssociation_4002OutgoingLinks(view);
			case ManyToOneAssociationEditPart.VISUAL_ID:
				return getManyToOneAssociation_4004OutgoingLinks(view);
			case OneToManyAssociationEditPart.VISUAL_ID:
				return getOneToManyAssociation_4006OutgoingLinks(view);
			case EnumAssociationEditPart.VISUAL_ID:
				return getEnumAssociation_4001OutgoingLinks(view);
			case DomainObjectInheritanceEditPart.VISUAL_ID:
				return getDomainObjectInheritance_4005OutgoingLinks(view);
		}

		return Collections.emptyList();
	}

	/**
	 * @param view
	 * @return an empty list
	 */
	@SuppressWarnings("unused")
	public static List<CodeCadenzaLinkDescriptor> getDomainNamespace_1000ContainedLinks(View view) {
		return Collections.emptyList();
	}

	/**
	 * @param view
	 * @return a list containing domain object links
	 */
	public static List<CodeCadenzaLinkDescriptor> getDomaiObject_2001ContainedLinks(View view) {
		final var result = new LinkedList<>(getContainedTypeModelFacetLinks_OneToOneAssociation_4003(view));
		result.addAll(getContainedTypeModelFacetLinks_ManyToManyAssociation_4002(view));
		result.addAll(getContainedTypeModelFacetLinks_ManyToOneAssociation_4004(view));
		result.addAll(getContainedTypeModelFacetLinks_OneToManyAssociation_4006(view));
		result.addAll(getContainedTypeModelFacetLinks_EnumAssociation_4001(view));
		result.addAll(getContainedTypeModelFacetLinks_DomainInheritance_4005(view));

		return result;
	}

	/**
	 * @param view
	 * @return an empty list
	 */
	@SuppressWarnings("unused")
	public static List<CodeCadenzaLinkDescriptor> getJavaEnum_2002ContainedLinks(View view) {
		return Collections.emptyList();
	}

	/**
	 * @param view
	 * @return an empty list
	 */
	@SuppressWarnings("unused")
	public static List<CodeCadenzaLinkDescriptor> getDomainAttribute_3001ContainedLinks(View view) {
		return Collections.emptyList();
	}

	/**
	 * @param view
	 * @return an empty list
	 */
	@SuppressWarnings("unused")
	public static List<CodeCadenzaLinkDescriptor> getEnumLiteral_3002ContainedLinks(View view) {
		return Collections.emptyList();
	}

	/**
	 * @param view
	 * @return an empty list
	 */
	@SuppressWarnings("unused")
	public static List<CodeCadenzaLinkDescriptor> getOneToOneAssociation_4003ContainedLinks(View view) {
		return Collections.emptyList();
	}

	/**
	 * @param view
	 * @return an empty list
	 */
	@SuppressWarnings("unused")
	public static List<CodeCadenzaLinkDescriptor> getManyToManyAssociation_4002ContainedLinks(View view) {
		return Collections.emptyList();
	}

	/**
	 * @param view
	 * @return an empty list
	 */
	@SuppressWarnings("unused")
	public static List<CodeCadenzaLinkDescriptor> getManyToOneAssociation_4004ContainedLinks(View view) {
		return Collections.emptyList();
	}

	/**
	 * @param view
	 * @return an empty list
	 */
	@SuppressWarnings("unused")
	public static List<CodeCadenzaLinkDescriptor> getOneToManyAssociation_4006ContainedLinks(View view) {
		return Collections.emptyList();
	}

	/**
	 * @param view
	 * @return an empty list
	 */
	@SuppressWarnings("unused")
	public static List<CodeCadenzaLinkDescriptor> getEnumAssociation_4001ContainedLinks(View view) {
		return Collections.emptyList();
	}

	/**
	 * @param view
	 * @return an empty list
	 */
	@SuppressWarnings("unused")
	public static List<CodeCadenzaLinkDescriptor> getDomainInheritance_4005ContainedLinks(View view) {
		return Collections.emptyList();
	}

	/**
	 * @param view
	 * @return a list containing all incoming links
	 */
	public static List<CodeCadenzaLinkDescriptor> getDomainObject_2001IncomingLinks(View view) {
		final var modelElement = (DomainObject) view.getElement();
		final Map<EObject, Collection<EStructuralFeature.Setting>> crossReferences = EcoreUtil.CrossReferencer
				.find(view.eResource().getResourceSet().getResources());

		final var result = new LinkedList<>(getIncomingTypeModelFacetLinks_OneToOneAssociation_4003(modelElement, crossReferences));
		result.addAll(getIncomingTypeModelFacetLinks_ManyToManyAssociation_4002(modelElement, crossReferences));
		result.addAll(getIncomingTypeModelFacetLinks_ManyToOneAssociation_4004(modelElement, crossReferences));
		result.addAll(getIncomingTypeModelFacetLinks_OneToManyAssociation_4006(modelElement, crossReferences));
		result.addAll(getIncomingTypeModelFacetLinks_DomainInheritance_4005(modelElement, crossReferences));

		return result;
	}

	/**
	 * @param view
	 * @return a list of incoming links for Java enumerations
	 */
	public static List<CodeCadenzaLinkDescriptor> getJavaEnum_2002IncomingLinks(View view) {
		final var modelElement = (JavaEnum) view.getElement();
		final Map<EObject, Collection<EStructuralFeature.Setting>> crossReferences = EcoreUtil.CrossReferencer
				.find(view.eResource().getResourceSet().getResources());

		return new LinkedList<>(getIncomingTypeModelFacetLinks_EnumAssociation_4001(modelElement, crossReferences));
	}

	/**
	 * @param view
	 * @return an empty list
	 */
	@SuppressWarnings("unused")
	public static List<CodeCadenzaLinkDescriptor> getDomainAttribute_3001IncomingLinks(View view) {
		return Collections.emptyList();
	}

	/**
	 * @param view
	 * @return an empty list
	 */
	@SuppressWarnings("unused")
	public static List<CodeCadenzaLinkDescriptor> getEnumLiteral_3002IncomingLinks(View view) {
		return Collections.emptyList();
	}

	/**
	 * @param view
	 * @return an empty list
	 */
	@SuppressWarnings("unused")
	public static List<CodeCadenzaLinkDescriptor> getOneToOneAssociation_4003IncomingLinks(View view) {
		return Collections.emptyList();
	}

	/**
	 * @param view
	 * @return an empty list
	 */
	@SuppressWarnings("unused")
	public static List<CodeCadenzaLinkDescriptor> getManyToManyAssociation_4002IncomingLinks(View view) {
		return Collections.emptyList();
	}

	/**
	 * @param view
	 * @return an empty list
	 */
	@SuppressWarnings("unused")
	public static List<CodeCadenzaLinkDescriptor> getManyToOneAssociation_4004IncomingLinks(View view) {
		return Collections.emptyList();
	}

	/**
	 * @param view
	 * @return an empty list
	 */
	@SuppressWarnings("unused")
	public static List<CodeCadenzaLinkDescriptor> getOneToManyAssociation_4006IncomingLinks(View view) {
		return Collections.emptyList();
	}

	/**
	 * @param view
	 * @return an empty list
	 */
	@SuppressWarnings("unused")
	public static List<CodeCadenzaLinkDescriptor> getEnumAssociation_4001IncomingLinks(View view) {
		return Collections.emptyList();
	}

	/**
	 * @param view
	 * @return an empty list
	 */
	@SuppressWarnings("unused")
	public static List<CodeCadenzaLinkDescriptor> getDomainObjectInheritance_4005IncomingLinks(View view) {
		return Collections.emptyList();
	}

	/**
	 * @param view
	 * @return a list containing all outgoing links
	 */
	public static List<CodeCadenzaLinkDescriptor> getDomainObject_2001OutgoingLinks(View view) {
		final var modelElement = (DomainObject) view.getElement();

		final var result = new LinkedList<>(getOutgoingTypeModelFacetLinks_OneToOneAssociation_4003(modelElement));
		result.addAll(getOutgoingTypeModelFacetLinks_ManyToManyAssociation_4002(modelElement));
		result.addAll(getOutgoingTypeModelFacetLinks_ManyToOneAssociation_4004(modelElement));
		result.addAll(getOutgoingTypeModelFacetLinks_OneToManyAssociation_4006(modelElement));
		result.addAll(getOutgoingTypeModelFacetLinks_EnumAssociation_4001(modelElement));
		result.addAll(getOutgoingTypeModelFacetLinks_DomainInheritance_4005(modelElement));

		return result;
	}

	/**
	 * @param view
	 * @return an empty list
	 */
	@SuppressWarnings("unused")
	public static List<CodeCadenzaLinkDescriptor> getJavaEnum_2002OutgoingLinks(View view) {
		return Collections.emptyList();
	}

	/**
	 * @param view
	 * @return an empty list
	 */
	@SuppressWarnings("unused")
	public static List<CodeCadenzaLinkDescriptor> getDomainAttribute_3001OutgoingLinks(View view) {
		return Collections.emptyList();
	}

	/**
	 * @param view
	 * @return an empty list
	 */
	@SuppressWarnings("unused")
	public static List<CodeCadenzaLinkDescriptor> getEnumLiteral_3002OutgoingLinks(View view) {
		return Collections.emptyList();
	}

	/**
	 * @param view
	 * @return an empty list
	 */
	@SuppressWarnings("unused")
	public static List<CodeCadenzaLinkDescriptor> getOneToOneAssociation_4003OutgoingLinks(View view) {
		return Collections.emptyList();
	}

	/**
	 * @param view
	 * @return an empty list
	 */
	@SuppressWarnings("unused")
	public static List<CodeCadenzaLinkDescriptor> getManyToManyAssociation_4002OutgoingLinks(View view) {
		return Collections.emptyList();
	}

	/**
	 * @param view
	 * @return an empty list
	 */
	@SuppressWarnings("unused")
	public static List<CodeCadenzaLinkDescriptor> getManyToOneAssociation_4004OutgoingLinks(View view) {
		return Collections.emptyList();
	}

	/**
	 * @param view
	 * @return an empty list
	 */
	@SuppressWarnings("unused")
	public static List<CodeCadenzaLinkDescriptor> getOneToManyAssociation_4006OutgoingLinks(View view) {
		return Collections.emptyList();
	}

	/**
	 * @param view
	 * @return an empty list
	 */
	@SuppressWarnings("unused")
	public static List<CodeCadenzaLinkDescriptor> getEnumAssociation_4001OutgoingLinks(View view) {
		return Collections.emptyList();
	}

	/**
	 * @param view
	 * @return an empty list
	 */
	@SuppressWarnings("unused")
	public static List<CodeCadenzaLinkDescriptor> getDomainObjectInheritance_4005OutgoingLinks(View view) {
		return Collections.emptyList();
	}

	/**
	 * @param view
	 * @return a list containing links
	 */
	private static Collection<CodeCadenzaLinkDescriptor> getContainedTypeModelFacetLinks_OneToOneAssociation_4003(View view) {
		final var result = new LinkedList<CodeCadenzaLinkDescriptor>();
		final var container = (DomainObject) view.getElement();
		final boolean isShortCut = view.getEAnnotation("Shortcut") != null;

		for (final AbstractDomainAssociation linkObject : container.getAssociations()) {
			if (!(linkObject instanceof final OneToOneAssociation link))
				continue;

			if (OneToOneAssociationEditPart.VISUAL_ID != CodeCadenzaVisualIDRegistry.getLinkWithClassVisualID(link))
				continue;

			final DomainObject dst = link.getTarget();
			final DomainObject src = link.getDomainObject();

			// Skip unnecessary shortcut links!
			if (!link.isOwner() || (isShortCut && dst.getNamespace().equals(src.getNamespace())))
				continue;

			result.add(new CodeCadenzaLinkDescriptor(src, dst, link, CodeCadenzaElementTypes.OneToOneAssociation_4003,
					OneToOneAssociationEditPart.VISUAL_ID));
		}

		return result;
	}

	/**
	 * @param view
	 * @return a list containing links
	 */
	private static Collection<CodeCadenzaLinkDescriptor> getContainedTypeModelFacetLinks_ManyToManyAssociation_4002(View view) {
		final var result = new LinkedList<CodeCadenzaLinkDescriptor>();
		final var container = (DomainObject) view.getElement();
		final boolean isShortCut = view.getEAnnotation("Shortcut") != null;

		for (final AbstractDomainAssociation linkObject : container.getAssociations()) {
			if (!(linkObject instanceof final ManyToManyAssociation link))
				continue;

			if (ManyToManyAssociationEditPart.VISUAL_ID != CodeCadenzaVisualIDRegistry.getLinkWithClassVisualID(link))
				continue;

			final DomainObject dst = link.getTarget();
			final DomainObject src = link.getDomainObject();

			// Skip unnecessary shortcut links!
			if (!link.isOwner() || (isShortCut && dst.getNamespace().equals(src.getNamespace())))
				continue;

			result.add(new CodeCadenzaLinkDescriptor(src, dst, link, CodeCadenzaElementTypes.ManyToManyAssociation_4002,
					ManyToManyAssociationEditPart.VISUAL_ID));
		}

		return result;
	}

	/**
	 * @param view
	 * @return a list containing links
	 */
	private static Collection<CodeCadenzaLinkDescriptor> getContainedTypeModelFacetLinks_ManyToOneAssociation_4004(View view) {
		final var result = new LinkedList<CodeCadenzaLinkDescriptor>();
		final var container = (DomainObject) view.getElement();
		final boolean isShortCut = view.getEAnnotation("Shortcut") != null;

		for (final AbstractDomainAssociation linkObject : container.getAssociations()) {
			if (!(linkObject instanceof final ManyToOneAssociation link))
				continue;

			if (ManyToOneAssociationEditPart.VISUAL_ID != CodeCadenzaVisualIDRegistry.getLinkWithClassVisualID(link))
				continue;

			final DomainObject dst = link.getTarget();
			final DomainObject src = link.getDomainObject();

			// Skip unnecessary shortcut links!
			if (link.getReverseAssociation() != null || (isShortCut && dst.getNamespace().equals(src.getNamespace())))
				continue;

			result.add(new CodeCadenzaLinkDescriptor(src, dst, link, CodeCadenzaElementTypes.ManyToOneAssociation_4004,
					ManyToOneAssociationEditPart.VISUAL_ID));
		}

		return result;
	}

	/**
	 * @param view
	 * @return a list containing links
	 */
	private static Collection<CodeCadenzaLinkDescriptor> getContainedTypeModelFacetLinks_OneToManyAssociation_4006(View view) {
		final var result = new LinkedList<CodeCadenzaLinkDescriptor>();
		final var container = (DomainObject) view.getElement();
		final boolean isShortCut = view.getEAnnotation("Shortcut") != null;

		for (final AbstractDomainAssociation linkObject : container.getAssociations()) {
			if (!(linkObject instanceof final OneToManyAssociation link))
				continue;

			if (OneToManyAssociationEditPart.VISUAL_ID != CodeCadenzaVisualIDRegistry.getLinkWithClassVisualID(link))
				continue;

			final DomainObject dst = link.getTarget();
			final DomainObject src = link.getDomainObject();

			// Skip unnecessary shortcut links!
			if (isShortCut && dst.getNamespace().equals(src.getNamespace()))
				continue;

			result.add(new CodeCadenzaLinkDescriptor(src, dst, link, CodeCadenzaElementTypes.OneToManyAssociation_4006,
					OneToManyAssociationEditPart.VISUAL_ID));
		}

		return result;
	}

	/**
	 * @param view
	 * @return a list containing links
	 */
	private static Collection<CodeCadenzaLinkDescriptor> getContainedTypeModelFacetLinks_EnumAssociation_4001(View view) {
		final var result = new LinkedList<CodeCadenzaLinkDescriptor>();
		final var container = (DomainObject) view.getElement();
		final boolean isShortCut = view.getEAnnotation("Shortcut") != null;

		for (final EnumAssociation link : container.getEnumAssociations()) {
			if (EnumAssociationEditPart.VISUAL_ID != CodeCadenzaVisualIDRegistry.getLinkWithClassVisualID(link))
				continue;

			final JavaEnum dst = link.getTarget();
			final DomainObject src = link.getSource();

			// Skip unnecessary shortcut links!
			if (isShortCut)
				continue;

			result.add(new CodeCadenzaLinkDescriptor(src, dst, link, CodeCadenzaElementTypes.EnumAssociation_4001,
					EnumAssociationEditPart.VISUAL_ID));
		}

		return result;
	}

	/**
	 * @param view
	 * @return a list containing links
	 */
	private static Collection<CodeCadenzaLinkDescriptor> getContainedTypeModelFacetLinks_DomainInheritance_4005(View view) {
		final var result = new LinkedList<CodeCadenzaLinkDescriptor>();
		final var container = (DomainObject) view.getElement();
		final boolean isShortCut = view.getEAnnotation("Shortcut") != null;
		final DomainInheritance link = container.getInheritance();

		if (DomainObjectInheritanceEditPart.VISUAL_ID != CodeCadenzaVisualIDRegistry.getLinkWithClassVisualID(link))
			return result;

		final DomainObject dst = link.getTarget();
		final DomainObject src = link.getSource();

		// Skip unnecessary shortcut links!
		if (isShortCut)
			return result;

		result.add(new CodeCadenzaLinkDescriptor(src, dst, link, CodeCadenzaElementTypes.DomainInheritance_4005,
				DomainObjectInheritanceEditPart.VISUAL_ID));

		return result;
	}

	/**
	 * @param target
	 * @param crossReferences
	 * @return a list containing links
	 */
	private static Collection<CodeCadenzaLinkDescriptor> getIncomingTypeModelFacetLinks_OneToOneAssociation_4003(
			DomainObject target, Map<EObject, Collection<EStructuralFeature.Setting>> crossReferences) {
		final var result = new LinkedList<CodeCadenzaLinkDescriptor>();
		final Collection<EStructuralFeature.Setting> settings = crossReferences.get(target);

		for (final EStructuralFeature.Setting setting : settings) {
			if (setting.getEStructuralFeature() != DomainPackage.eINSTANCE.getAbstractDomainAssociation_Target()
					|| !(setting.getEObject() instanceof final OneToOneAssociation link))
				continue;

			if (OneToOneAssociationEditPart.VISUAL_ID != CodeCadenzaVisualIDRegistry.getLinkWithClassVisualID(link))
				continue;

			final DomainObject src = link.getDomainObject();

			result.add(new CodeCadenzaLinkDescriptor(src, target, link, CodeCadenzaElementTypes.OneToOneAssociation_4003,
					OneToOneAssociationEditPart.VISUAL_ID));
		}

		return result;
	}

	/**
	 * @param target
	 * @param crossReferences
	 * @return a list containing links
	 */
	private static Collection<CodeCadenzaLinkDescriptor> getIncomingTypeModelFacetLinks_ManyToManyAssociation_4002(
			DomainObject target, Map<EObject, Collection<EStructuralFeature.Setting>> crossReferences) {
		final var result = new LinkedList<CodeCadenzaLinkDescriptor>();
		final Collection<EStructuralFeature.Setting> settings = crossReferences.get(target);

		for (final EStructuralFeature.Setting setting : settings) {
			if (setting.getEStructuralFeature() != DomainPackage.eINSTANCE.getAbstractDomainAssociation_Target()
					|| !(setting.getEObject() instanceof final ManyToManyAssociation link))
				continue;

			if (ManyToManyAssociationEditPart.VISUAL_ID != CodeCadenzaVisualIDRegistry.getLinkWithClassVisualID(link))
				continue;

			final DomainObject src = link.getDomainObject();

			result.add(new CodeCadenzaLinkDescriptor(src, target, link, CodeCadenzaElementTypes.ManyToManyAssociation_4002,
					ManyToManyAssociationEditPart.VISUAL_ID));
		}

		return result;
	}

	/**
	 * @param target
	 * @param crossReferences
	 * @return a list containing links
	 */
	private static Collection<CodeCadenzaLinkDescriptor> getIncomingTypeModelFacetLinks_ManyToOneAssociation_4004(
			DomainObject target, Map<EObject, Collection<EStructuralFeature.Setting>> crossReferences) {
		final var result = new LinkedList<CodeCadenzaLinkDescriptor>();
		final Collection<EStructuralFeature.Setting> settings = crossReferences.get(target);

		for (final EStructuralFeature.Setting setting : settings) {
			if (setting.getEStructuralFeature() != DomainPackage.eINSTANCE.getAbstractDomainAssociation_Target()
					|| !(setting.getEObject() instanceof final ManyToOneAssociation link))
				continue;

			if (ManyToOneAssociationEditPart.VISUAL_ID != CodeCadenzaVisualIDRegistry.getLinkWithClassVisualID(link))
				continue;

			final DomainObject src = link.getDomainObject();

			result.add(new CodeCadenzaLinkDescriptor(src, target, link, CodeCadenzaElementTypes.ManyToOneAssociation_4004,
					ManyToOneAssociationEditPart.VISUAL_ID));
		}

		return result;
	}

	/**
	 * @param target
	 * @param crossReferences
	 * @return a list containing links
	 */
	private static Collection<CodeCadenzaLinkDescriptor> getIncomingTypeModelFacetLinks_OneToManyAssociation_4006(
			DomainObject target, Map<EObject, Collection<EStructuralFeature.Setting>> crossReferences) {
		final var result = new LinkedList<CodeCadenzaLinkDescriptor>();
		final Collection<EStructuralFeature.Setting> settings = crossReferences.get(target);

		for (final EStructuralFeature.Setting setting : settings) {
			if (setting.getEStructuralFeature() != DomainPackage.eINSTANCE.getAbstractDomainAssociation_Target()
					|| !(setting.getEObject() instanceof final OneToManyAssociation link))
				continue;

			if (OneToManyAssociationEditPart.VISUAL_ID != CodeCadenzaVisualIDRegistry.getLinkWithClassVisualID(link))
				continue;

			final DomainObject src = link.getDomainObject();

			result.add(new CodeCadenzaLinkDescriptor(src, target, link, CodeCadenzaElementTypes.OneToManyAssociation_4006,
					OneToManyAssociationEditPart.VISUAL_ID));
		}

		return result;
	}

	/**
	 * @param target
	 * @param crossReferences
	 * @return a list containing links
	 */
	private static Collection<CodeCadenzaLinkDescriptor> getIncomingTypeModelFacetLinks_EnumAssociation_4001(JavaEnum target,
			Map<EObject, Collection<EStructuralFeature.Setting>> crossReferences) {
		final var result = new LinkedList<CodeCadenzaLinkDescriptor>();
		final Collection<EStructuralFeature.Setting> settings = crossReferences.get(target);

		for (final EStructuralFeature.Setting setting : settings) {
			if (setting.getEStructuralFeature() != DomainPackage.eINSTANCE.getEnumAssociation_Target()
					|| !(setting.getEObject() instanceof final EnumAssociation link))
				continue;

			if (EnumAssociationEditPart.VISUAL_ID != CodeCadenzaVisualIDRegistry.getLinkWithClassVisualID(link))
				continue;

			final DomainObject src = link.getSource();

			result.add(new CodeCadenzaLinkDescriptor(src, target, link, CodeCadenzaElementTypes.EnumAssociation_4001,
					EnumAssociationEditPart.VISUAL_ID));
		}

		return result;
	}

	/**
	 * @param target
	 * @param crossReferences
	 * @return a list containing links
	 */
	private static Collection<CodeCadenzaLinkDescriptor> getIncomingTypeModelFacetLinks_DomainInheritance_4005(DomainObject target,
			Map<EObject, Collection<EStructuralFeature.Setting>> crossReferences) {
		final var result = new LinkedList<CodeCadenzaLinkDescriptor>();
		final Collection<EStructuralFeature.Setting> settings = crossReferences.get(target);

		for (final EStructuralFeature.Setting setting : settings) {
			if (setting.getEStructuralFeature() != DomainPackage.eINSTANCE.getAbstractDomainAssociation_Target()
					|| !(setting.getEObject() instanceof final DomainInheritance link))
				continue;

			if (DomainObjectInheritanceEditPart.VISUAL_ID != CodeCadenzaVisualIDRegistry.getLinkWithClassVisualID(link))
				continue;

			final DomainObject src = link.getSource();

			result.add(new CodeCadenzaLinkDescriptor(src, target, link, CodeCadenzaElementTypes.DomainInheritance_4005,
					DomainObjectInheritanceEditPart.VISUAL_ID));
		}

		return result;
	}

	/**
	 * @param source
	 * @return a list containing outgoing links
	 */
	private static Collection<CodeCadenzaLinkDescriptor> getOutgoingTypeModelFacetLinks_OneToOneAssociation_4003(
			DomainObject source) {
		DomainObject container = null;

		// Find a container element for the new link. Climb up the containment hierarchy starting from the source and return the first
		// element that is an instance of the container class.
		for (EObject element = source; element != null && container == null; element = element.eContainer())
			if (element instanceof final DomainObject domainObject)
				container = domainObject;

		if (container == null)
			return Collections.emptyList();

		final var result = new LinkedList<CodeCadenzaLinkDescriptor>();

		for (final AbstractDomainAssociation linkObject : container.getAssociations()) {
			if (!(linkObject instanceof final OneToOneAssociation link))
				continue;

			if (OneToOneAssociationEditPart.VISUAL_ID != CodeCadenzaVisualIDRegistry.getLinkWithClassVisualID(link))
				continue;

			final DomainObject dst = link.getTarget();
			final DomainObject src = link.getDomainObject();

			if (src != source)
				continue;

			result.add(new CodeCadenzaLinkDescriptor(src, dst, link, CodeCadenzaElementTypes.OneToOneAssociation_4003,
					OneToOneAssociationEditPart.VISUAL_ID));
		}

		return result;
	}

	/**
	 * @param source
	 * @return a list containing outgoing links
	 */
	private static Collection<CodeCadenzaLinkDescriptor> getOutgoingTypeModelFacetLinks_ManyToManyAssociation_4002(
			DomainObject source) {
		DomainObject container = null;

		// Find a container element for the new link. Climb up the containment hierarchy starting from the source and return the first
		// element that is an instance of the container class.
		for (EObject element = source; element != null && container == null; element = element.eContainer())
			if (element instanceof final DomainObject domainObject)
				container = domainObject;

		if (container == null)
			return Collections.emptyList();

		final var result = new LinkedList<CodeCadenzaLinkDescriptor>();

		for (final AbstractDomainAssociation linkObject : container.getAssociations()) {
			if (!(linkObject instanceof final ManyToManyAssociation link))
				continue;

			if (ManyToManyAssociationEditPart.VISUAL_ID != CodeCadenzaVisualIDRegistry.getLinkWithClassVisualID(link))
				continue;

			final DomainObject dst = link.getTarget();
			final DomainObject src = link.getDomainObject();

			if (src != source)
				continue;

			result.add(new CodeCadenzaLinkDescriptor(src, dst, link, CodeCadenzaElementTypes.ManyToManyAssociation_4002,
					ManyToManyAssociationEditPart.VISUAL_ID));
		}

		return result;
	}

	/**
	 * @param source
	 * @return a list containing outgoing links
	 */
	private static Collection<CodeCadenzaLinkDescriptor> getOutgoingTypeModelFacetLinks_ManyToOneAssociation_4004(
			DomainObject source) {
		DomainObject container = null;

		// Find a container element for the new link. Climb up the containment hierarchy starting from the source and return the first
		// element that is an instance of the container class.
		for (EObject element = source; element != null && container == null; element = element.eContainer())
			if (element instanceof final DomainObject domainObject)
				container = domainObject;

		if (container == null)
			return Collections.emptyList();

		final var result = new LinkedList<CodeCadenzaLinkDescriptor>();

		for (final AbstractDomainAssociation linkObject : container.getAssociations()) {
			if (!(linkObject instanceof final ManyToOneAssociation link))
				continue;

			if (ManyToOneAssociationEditPart.VISUAL_ID != CodeCadenzaVisualIDRegistry.getLinkWithClassVisualID(link))
				continue;

			final DomainObject dst = link.getTarget();
			final DomainObject src = link.getDomainObject();

			if (src != source)
				continue;

			result.add(new CodeCadenzaLinkDescriptor(src, dst, link, CodeCadenzaElementTypes.ManyToOneAssociation_4004,
					ManyToOneAssociationEditPart.VISUAL_ID));
		}

		return result;
	}

	/**
	 * @param source
	 * @return a list containing outgoing links
	 */
	private static Collection<CodeCadenzaLinkDescriptor> getOutgoingTypeModelFacetLinks_OneToManyAssociation_4006(
			DomainObject source) {
		DomainObject container = null;

		// Find a container element for the new link. Climb up the containment hierarchy starting from the source and return the first
		// element that is an instance of the container class.
		for (EObject element = source; element != null && container == null; element = element.eContainer())
			if (element instanceof final DomainObject domainObject)
				container = domainObject;

		if (container == null)
			return Collections.emptyList();

		final var result = new LinkedList<CodeCadenzaLinkDescriptor>();

		for (final AbstractDomainAssociation linkObject : container.getAssociations()) {
			if (!(linkObject instanceof final OneToManyAssociation link))
				continue;

			if (OneToManyAssociationEditPart.VISUAL_ID != CodeCadenzaVisualIDRegistry.getLinkWithClassVisualID(link))
				continue;

			final DomainObject dst = link.getTarget();
			final DomainObject src = link.getDomainObject();

			if (src != source)
				continue;

			result.add(new CodeCadenzaLinkDescriptor(src, dst, link, CodeCadenzaElementTypes.OneToManyAssociation_4006,
					OneToManyAssociationEditPart.VISUAL_ID));
		}

		return result;
	}

	/**
	 * @param source
	 * @return a list containing outgoing links
	 */
	private static Collection<CodeCadenzaLinkDescriptor> getOutgoingTypeModelFacetLinks_EnumAssociation_4001(DomainObject source) {
		DomainObject container = null;

		// Find a container element for the new link. Climb up the containment hierarchy starting from the source and return the first
		// element that is an instance of the container class.
		for (EObject element = source; element != null && container == null; element = element.eContainer())
			if (element instanceof final DomainObject domainObject)
				container = domainObject;

		if (container == null)
			return Collections.emptyList();

		final var result = new LinkedList<CodeCadenzaLinkDescriptor>();

		for (final EnumAssociation link : container.getEnumAssociations()) {
			if (EnumAssociationEditPart.VISUAL_ID != CodeCadenzaVisualIDRegistry.getLinkWithClassVisualID(link))
				continue;

			final JavaEnum dst = link.getTarget();
			final DomainObject src = link.getSource();

			if (src != source)
				continue;

			result.add(new CodeCadenzaLinkDescriptor(src, dst, link, CodeCadenzaElementTypes.EnumAssociation_4001,
					EnumAssociationEditPart.VISUAL_ID));
		}

		return result;
	}

	/**
	 * @param source
	 * @return a list containing outgoing links
	 */
	private static Collection<CodeCadenzaLinkDescriptor> getOutgoingTypeModelFacetLinks_DomainInheritance_4005(
			DomainObject source) {
		DomainObject container = null;

		// Find a container element for the new link. Climb up the containment hierarchy starting from the source and return the first
		// element that is an instance of the container class.
		for (EObject element = source; element != null && container == null; element = element.eContainer())
			if (element instanceof final DomainObject domainObject)
				container = domainObject;

		if (container == null)
			return Collections.emptyList();

		final var result = new LinkedList<CodeCadenzaLinkDescriptor>();
		final DomainInheritance link = container.getInheritance();

		if (DomainObjectInheritanceEditPart.VISUAL_ID != CodeCadenzaVisualIDRegistry.getLinkWithClassVisualID(link))
			return result;

		final DomainObject dst = link.getTarget();
		final DomainObject src = link.getSource();

		if (src != source)
			return result;

		result.add(new CodeCadenzaLinkDescriptor(src, dst, link, CodeCadenzaElementTypes.DomainInheritance_4005,
				DomainObjectInheritanceEditPart.VISUAL_ID));

		return result;
	}

}

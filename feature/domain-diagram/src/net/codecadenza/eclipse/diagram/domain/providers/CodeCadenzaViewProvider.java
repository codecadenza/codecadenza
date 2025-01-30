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
package net.codecadenza.eclipse.diagram.domain.providers;

import net.codecadenza.eclipse.diagram.domain.edit.parts.DomainAttributeCompartmentEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.DomainAttributeEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.DomainAttributeNameEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.DomainNamespaceEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.DomainObjectEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.DomainObjectInheritanceEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.DomainObjectNameEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.EnumAssociationEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.EnumLiteralEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.EnumLiteralNameEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.JavaEnumEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.JavaEnumEnumerationLiteralCompartmentEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.JavaEnumNameEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.ManyToManyAssociationEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.ManyToOneAssociationEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.OneToManyAssociationEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.OneToOneAssociationEditPart;
import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaVisualIDRegistry;
import net.codecadenza.eclipse.diagram.domain.view.factories.DomainAttributeCompartmentViewFactory;
import net.codecadenza.eclipse.diagram.domain.view.factories.DomainAttributeNameViewFactory;
import net.codecadenza.eclipse.diagram.domain.view.factories.DomainAttributeViewFactory;
import net.codecadenza.eclipse.diagram.domain.view.factories.DomainInheritanceViewFactory;
import net.codecadenza.eclipse.diagram.domain.view.factories.DomainNameViewFactory;
import net.codecadenza.eclipse.diagram.domain.view.factories.DomainNamespaceViewFactory;
import net.codecadenza.eclipse.diagram.domain.view.factories.DomainObjectViewFactory;
import net.codecadenza.eclipse.diagram.domain.view.factories.EnumAssociationViewFactory;
import net.codecadenza.eclipse.diagram.domain.view.factories.EnumLiteralNameViewFactory;
import net.codecadenza.eclipse.diagram.domain.view.factories.EnumLiteralViewFactory;
import net.codecadenza.eclipse.diagram.domain.view.factories.JavaEnumEnumerationLiteralCompartmentViewFactory;
import net.codecadenza.eclipse.diagram.domain.view.factories.JavaEnumNameViewFactory;
import net.codecadenza.eclipse.diagram.domain.view.factories.JavaEnumViewFactory;
import net.codecadenza.eclipse.diagram.domain.view.factories.ManyToManyAssociationViewFactory;
import net.codecadenza.eclipse.diagram.domain.view.factories.ManyToOneAssociationViewFactory;
import net.codecadenza.eclipse.diagram.domain.view.factories.OneToManyAssociationViewFactory;
import net.codecadenza.eclipse.diagram.domain.view.factories.OneToOneAssociationViewFactory;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.gmf.runtime.diagram.core.providers.AbstractViewProvider;
import org.eclipse.gmf.runtime.emf.type.core.IElementType;
import org.eclipse.gmf.runtime.emf.type.core.IHintedType;
import org.eclipse.gmf.runtime.notation.View;

/**
 * <p>
 * View provider
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CodeCadenzaViewProvider extends AbstractViewProvider {
	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.core.providers.AbstractViewProvider#getDiagramViewClass(org.eclipse.core.runtime.
	 * IAdaptable, java.lang.String)
	 */
	@Override
	protected Class<?> getDiagramViewClass(IAdaptable semanticAdapter, String diagramKind) {
		final EObject semanticElement = getSemanticElement(semanticAdapter);

		if (DomainNamespaceEditPart.MODEL_ID.equals(diagramKind) && CodeCadenzaVisualIDRegistry.getDiagramVisualID(semanticElement) != -1)
			return DomainNamespaceViewFactory.class;

		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.core.providers.AbstractViewProvider#getNodeViewClass(org.eclipse.core.runtime.
	 * IAdaptable, org.eclipse.gmf.runtime.notation.View, java.lang.String)
	 */
	@Override
	protected Class<?> getNodeViewClass(IAdaptable semanticAdapter, View containerView, String semanticHint) {
		if (containerView == null)
			return null;

		final IElementType elementType = getSemanticElementType(semanticAdapter);
		final EObject domainElement = getSemanticElement(semanticAdapter);
		int visualID;

		if (semanticHint == null) {
			// The semantic hint is not specified. This can be a result of the call from the CanonicalEditPolicy. In this situation
			// there should be no element type. The visual ID will be determined by VisualIDRegistry.getNodeVisualID() for the domain
			// element.
			if (elementType != null || domainElement == null)
				return null;

			visualID = CodeCadenzaVisualIDRegistry.getNodeVisualID(containerView, domainElement);
		}
		else {
			visualID = CodeCadenzaVisualIDRegistry.getVisualID(semanticHint);

			if (elementType != null) {
				// The semantic hint is specified together with the element type. Both parameters should describe exactly the same diagram
				// element. In addition we check the visual ID that is returned by VisualIDRegistry.getNodeVisualID()
				if (!CodeCadenzaElementTypes.isKnownElementType(elementType) || !(elementType instanceof final IHintedType hintedElement))
					return null; // Foreign element type!

				final String elementTypeHint = hintedElement.getSemanticHint();

				if (!semanticHint.equals(elementTypeHint))
					return null; // If a semantic hint is specified it should be the same as the hint of the element type!

				if (domainElement != null && visualID != CodeCadenzaVisualIDRegistry.getNodeVisualID(containerView, domainElement))
					return null; // The visual ID of the node EClass should match the visual ID of the element type!
			}
			else {
				// The element type is not specified. The domain element should be present (except pure design elements).
				// This method is called with EObjectAdapter as parameter from:
				// - ViewService.createNode(View container, EObject eObject, String type, PreferencesHint preferencesHint)
				// - generated ViewFactory.decorateView() for a parent element
				if (!DomainNamespaceEditPart.MODEL_ID.equals(CodeCadenzaVisualIDRegistry.getModelID(containerView)))
					return null; // Foreign diagram!
				switch (visualID) {
					case DomainObjectEditPart.VISUAL_ID, JavaEnumEditPart.VISUAL_ID, DomainAttributeEditPart.VISUAL_ID, EnumLiteralEditPart.VISUAL_ID:
						if (domainElement == null || visualID != CodeCadenzaVisualIDRegistry.getNodeVisualID(containerView, domainElement))
							return null; // The visual ID in the semantic hint should match the visual ID of the domain element!

						break;
					case DomainObjectNameEditPart.VISUAL_ID, DomainAttributeCompartmentEditPart.VISUAL_ID:
						if (DomainObjectEditPart.VISUAL_ID != CodeCadenzaVisualIDRegistry.getVisualID(containerView)
								|| containerView.getElement() != domainElement)
							return null; // Wrong container!

						break;
					case JavaEnumNameEditPart.VISUAL_ID, JavaEnumEnumerationLiteralCompartmentEditPart.VISUAL_ID:
						if (JavaEnumEditPart.VISUAL_ID != CodeCadenzaVisualIDRegistry.getVisualID(containerView)
								|| containerView.getElement() != domainElement)
							return null; // Wrong container!

						break;
					case DomainAttributeNameEditPart.VISUAL_ID:
						if (DomainAttributeEditPart.VISUAL_ID != CodeCadenzaVisualIDRegistry.getVisualID(containerView)
								|| containerView.getElement() != domainElement)
							return null; // Wrong container!

						break;
					case EnumLiteralNameEditPart.VISUAL_ID:
						if (EnumLiteralEditPart.VISUAL_ID != CodeCadenzaVisualIDRegistry.getVisualID(containerView)
								|| containerView.getElement() != domainElement)
							return null; // Wrong container!

						break;
					default:
						return null;
				}
			}
		}

		return getNodeViewClass(containerView, visualID);
	}

	/**
	 * @param containerView
	 * @param visualID
	 * @return the class of the node view
	 */
	protected Class<?> getNodeViewClass(View containerView, int visualID) {
		if (containerView == null || !CodeCadenzaVisualIDRegistry.canCreateNode(containerView, visualID))
			return null;

		switch (visualID) {
			case DomainObjectEditPart.VISUAL_ID:
				return DomainObjectViewFactory.class;
			case DomainObjectNameEditPart.VISUAL_ID:
				return DomainNameViewFactory.class;
			case JavaEnumEditPart.VISUAL_ID:
				return JavaEnumViewFactory.class;
			case JavaEnumNameEditPart.VISUAL_ID:
				return JavaEnumNameViewFactory.class;
			case DomainAttributeEditPart.VISUAL_ID:
				return DomainAttributeViewFactory.class;
			case DomainAttributeNameEditPart.VISUAL_ID:
				return DomainAttributeNameViewFactory.class;
			case EnumLiteralEditPart.VISUAL_ID:
				return EnumLiteralViewFactory.class;
			case EnumLiteralNameEditPart.VISUAL_ID:
				return EnumLiteralNameViewFactory.class;
			case DomainAttributeCompartmentEditPart.VISUAL_ID:
				return DomainAttributeCompartmentViewFactory.class;
			case JavaEnumEnumerationLiteralCompartmentEditPart.VISUAL_ID:
				return JavaEnumEnumerationLiteralCompartmentViewFactory.class;
		}

		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.gmf.runtime.diagram.core.providers.AbstractViewProvider#getEdgeViewClass(org.eclipse.core.runtime.
	 * IAdaptable, org.eclipse.gmf.runtime.notation.View, java.lang.String)
	 */
	@Override
	protected Class<?> getEdgeViewClass(IAdaptable semanticAdapter, View containerView, String semanticHint) {
		final IElementType elementType = getSemanticElementType(semanticAdapter);

		if (!CodeCadenzaElementTypes.isKnownElementType(elementType) || !(elementType instanceof final IHintedType hintedElement))
			return null; // Foreign element type!

		final String elementTypeHint = hintedElement.getSemanticHint();

		if (elementTypeHint == null)
			return null; // The hint must be specified!

		if (semanticHint != null && !semanticHint.equals(elementTypeHint))
			return null; // If a semantic hint is specified it should be the same as the hint of the element type

		final int visualID = CodeCadenzaVisualIDRegistry.getVisualID(elementTypeHint);
		final EObject domainElement = getSemanticElement(semanticAdapter);

		if (domainElement != null && visualID != CodeCadenzaVisualIDRegistry.getLinkWithClassVisualID(domainElement))
			return null; // The visual ID in the semantic hint should match the visual ID of the domain element!

		return getEdgeViewClass(visualID);
	}

	/**
	 * @param visualID
	 * @return the class of the edge view
	 */
	protected Class<?> getEdgeViewClass(int visualID) {
		switch (visualID) {
			case OneToOneAssociationEditPart.VISUAL_ID:
				return OneToOneAssociationViewFactory.class;
			case ManyToManyAssociationEditPart.VISUAL_ID:
				return ManyToManyAssociationViewFactory.class;
			case ManyToOneAssociationEditPart.VISUAL_ID:
				return ManyToOneAssociationViewFactory.class;
			case OneToManyAssociationEditPart.VISUAL_ID:
				return OneToManyAssociationViewFactory.class;
			case EnumAssociationEditPart.VISUAL_ID:
				return EnumAssociationViewFactory.class;
			case DomainObjectInheritanceEditPart.VISUAL_ID:
				return DomainInheritanceViewFactory.class;
		}

		return null;
	}

	/**
	 * @param semanticAdapter
	 * @return the selected element type
	 */
	private IElementType getSemanticElementType(IAdaptable semanticAdapter) {
		if (semanticAdapter == null)
			return null;

		return semanticAdapter.getAdapter(IElementType.class);
	}

}

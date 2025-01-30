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
import net.codecadenza.eclipse.model.domain.DomainPackage;
import net.codecadenza.eclipse.model.java.JavaPackage;
import org.eclipse.core.runtime.Platform;
import org.eclipse.emf.ecore.EAnnotation;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.gmf.runtime.notation.Diagram;
import org.eclipse.gmf.runtime.notation.View;

/**
 * <p>
 * This registry is used to determine which type of visual object should be created for the corresponding Diagram, Node, ChildNode
 * or Link represented by a domain model object
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CodeCadenzaVisualIDRegistry {
	private static final String DEBUG_KEY = "net.codecadenza.eclipse.diagram/debug/visualID";

	/**
	 * Prevent instantiation
	 */
	private CodeCadenzaVisualIDRegistry() {

	}

	/**
	 * @param view
	 * @return the visual ID
	 */
	public static int getVisualID(View view) {
		if (view instanceof Diagram) {
			if (DomainNamespaceEditPart.MODEL_ID.equals(view.getType()))
				return DomainNamespaceEditPart.VISUAL_ID;

			return -1;
		}

		return CodeCadenzaVisualIDRegistry.getVisualID(view.getType());
	}

	/**
	 * @param view
	 * @return the model ID
	 */
	public static String getModelID(View view) {
		final View diagram = view.getDiagram();

		while (view != diagram) {
			final EAnnotation annotation = view.getEAnnotation("Shortcut");

			if (annotation != null)
				return annotation.getDetails().get("modelID");

			view = (View) view.eContainer();
		}

		return diagram != null ? diagram.getType() : null;
	}

	/**
	 * @param type
	 * @return the visual ID
	 */
	public static int getVisualID(String type) {
		try {
			return Integer.parseInt(type);
		}
		catch (final NumberFormatException e) {
			if (Boolean.TRUE.toString().equalsIgnoreCase(Platform.getDebugOption(DEBUG_KEY)))
				CodeCadenzaDiagramEditorPlugin.getInstance().logError("Unable to parse view type as a visualID number: " + type, null);
		}

		return -1;
	}

	/**
	 * @param visualID
	 * @return the type
	 */
	public static String getType(int visualID) {
		return String.valueOf(visualID);
	}

	/**
	 * @param domainElement
	 * @return the visual ID
	 */
	public static int getDiagramVisualID(EObject domainElement) {
		if (domainElement == null)
			return -1;

		if (DomainPackage.eINSTANCE.getDomainNamespace().isSuperTypeOf(domainElement.eClass()))
			return DomainNamespaceEditPart.VISUAL_ID;

		return -1;
	}

	/**
	 * @param containerView
	 * @param domainElement
	 * @return the visual ID of the node
	 */
	public static int getNodeVisualID(View containerView, EObject domainElement) {
		if (domainElement == null)
			return -1;

		final String containerModelID = CodeCadenzaVisualIDRegistry.getModelID(containerView);

		if (!DomainNamespaceEditPart.MODEL_ID.equals(containerModelID) && !DIAGRAM_FILE_EXTENSION.equals(containerModelID)
				&& !MODEL_FILE_EXTENSION.equals(containerModelID))
			return -1;

		int containerVisualID;

		if (DomainNamespaceEditPart.MODEL_ID.equals(containerModelID))
			containerVisualID = CodeCadenzaVisualIDRegistry.getVisualID(containerView);
		else if (containerView instanceof Diagram)
			containerVisualID = DomainNamespaceEditPart.VISUAL_ID;
		else
			return -1;

		switch (containerVisualID) {
			case DomainAttributeCompartmentEditPart.VISUAL_ID:
				if (DomainPackage.eINSTANCE.getDomainAttribute().isSuperTypeOf(domainElement.eClass()))
					return DomainAttributeEditPart.VISUAL_ID;

				break;
			case JavaEnumEnumerationLiteralCompartmentEditPart.VISUAL_ID:
				if (JavaPackage.eINSTANCE.getEnumLiteral().isSuperTypeOf(domainElement.eClass()))
					return EnumLiteralEditPart.VISUAL_ID;

				break;
			case DomainNamespaceEditPart.VISUAL_ID:
				if (DomainPackage.eINSTANCE.getDomainObject().isSuperTypeOf(domainElement.eClass()))
					return DomainObjectEditPart.VISUAL_ID;

				if (JavaPackage.eINSTANCE.getJavaEnum().isSuperTypeOf(domainElement.eClass()))
					return JavaEnumEditPart.VISUAL_ID;

				break;
		}

		return -1;
	}

	/**
	 * @param containerView
	 * @param nodeVisualID
	 * @return true if the node can be created
	 */
	public static boolean canCreateNode(View containerView, int nodeVisualID) {
		final String containerModelID = CodeCadenzaVisualIDRegistry.getModelID(containerView);

		if (!DomainNamespaceEditPart.MODEL_ID.equals(containerModelID) && !DIAGRAM_FILE_EXTENSION.equals(containerModelID)
				&& !MODEL_FILE_EXTENSION.equals(containerModelID))
			return false;

		int containerVisualID;

		if (DomainNamespaceEditPart.MODEL_ID.equals(containerModelID))
			containerVisualID = CodeCadenzaVisualIDRegistry.getVisualID(containerView);
		else if (containerView instanceof Diagram)
			containerVisualID = DomainNamespaceEditPart.VISUAL_ID;
		else
			return false;

		switch (containerVisualID) {
			case DomainObjectEditPart.VISUAL_ID:
				if (DomainObjectNameEditPart.VISUAL_ID == nodeVisualID)
					return true;

				if (DomainAttributeCompartmentEditPart.VISUAL_ID == nodeVisualID)
					return true;

				break;
			case JavaEnumEditPart.VISUAL_ID:
				if (JavaEnumNameEditPart.VISUAL_ID == nodeVisualID)
					return true;

				if (JavaEnumEnumerationLiteralCompartmentEditPart.VISUAL_ID == nodeVisualID)
					return true;

				break;
			case DomainAttributeEditPart.VISUAL_ID:
				if (DomainAttributeNameEditPart.VISUAL_ID == nodeVisualID)
					return true;

				break;
			case EnumLiteralEditPart.VISUAL_ID:
				if (EnumLiteralNameEditPart.VISUAL_ID == nodeVisualID)
					return true;

				break;
			case DomainAttributeCompartmentEditPart.VISUAL_ID:
				if (DomainAttributeEditPart.VISUAL_ID == nodeVisualID)
					return true;

				break;
			case JavaEnumEnumerationLiteralCompartmentEditPart.VISUAL_ID:
				if (EnumLiteralEditPart.VISUAL_ID == nodeVisualID)
					return true;

				break;
			case DomainNamespaceEditPart.VISUAL_ID:
				if (DomainObjectEditPart.VISUAL_ID == nodeVisualID)
					return true;

				if (JavaEnumEditPart.VISUAL_ID == nodeVisualID)
					return true;

				break;
		}

		return false;
	}

	/**
	 * @param domainElement
	 * @return the link
	 */
	public static int getLinkWithClassVisualID(EObject domainElement) {
		if (domainElement == null)
			return -1;

		if (DomainPackage.eINSTANCE.getOneToOneAssociation().isSuperTypeOf(domainElement.eClass()))
			return OneToOneAssociationEditPart.VISUAL_ID;

		if (DomainPackage.eINSTANCE.getManyToManyAssociation().isSuperTypeOf(domainElement.eClass()))
			return ManyToManyAssociationEditPart.VISUAL_ID;

		if (DomainPackage.eINSTANCE.getManyToOneAssociation().isSuperTypeOf(domainElement.eClass()))
			return ManyToOneAssociationEditPart.VISUAL_ID;

		if (DomainPackage.eINSTANCE.getOneToManyAssociation().isSuperTypeOf(domainElement.eClass()))
			return OneToManyAssociationEditPart.VISUAL_ID;

		if (DomainPackage.eINSTANCE.getEnumAssociation().isSuperTypeOf(domainElement.eClass()))
			return EnumAssociationEditPart.VISUAL_ID;

		if (DomainPackage.eINSTANCE.getDomainInheritance().isSuperTypeOf(domainElement.eClass()))
			return DomainObjectInheritanceEditPart.VISUAL_ID;

		return -1;
	}

}

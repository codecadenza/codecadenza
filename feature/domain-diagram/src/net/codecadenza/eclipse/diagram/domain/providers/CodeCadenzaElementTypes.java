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

import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.Map;
import java.util.Set;
import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaDiagramEditorPlugin;
import net.codecadenza.eclipse.model.domain.DomainPackage;
import net.codecadenza.eclipse.model.java.JavaPackage;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EClassifier;
import org.eclipse.emf.ecore.ENamedElement;
import org.eclipse.emf.ecore.EStructuralFeature;
import org.eclipse.gmf.runtime.emf.type.core.ElementTypeRegistry;
import org.eclipse.gmf.runtime.emf.type.core.IElementType;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.swt.graphics.Image;

/**
 * <p>
 * Element types
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CodeCadenzaElementTypes {
	private static Map<IElementType, EClass> elements;
	private static ImageRegistry imageRegistry;
	private static Set<IElementType> KNOWN_ELEMENT_TYPES;
	public static final IElementType DomainNamespace_1000 = getElementType("net.codecadenza.eclipse.diagram.DomainNamespace_1000");
	public static final IElementType DomainObject_2001 = getElementType("net.codecadenza.eclipse.diagram.DomainObject_2001");
	public static final IElementType JavaEnum_2002 = getElementType("net.codecadenza.eclipse.diagram.JavaEnum_2002");
	public static final IElementType DomainAttribute_3001 = getElementType("net.codecadenza.eclipse.diagram.DomainAttribute_3001");
	public static final IElementType EnumLiteral_3002 = getElementType("net.codecadenza.eclipse.diagram.EnumLiteral_3002");
	public static final IElementType OneToOneAssociation_4003 = getElementType(
			"net.codecadenza.eclipse.diagram.OneToOneAssociation_4003");
	public static final IElementType ManyToManyAssociation_4002 = getElementType(
			"net.codecadenza.eclipse.diagram.ManyToManyAssociation_4002");
	public static final IElementType ManyToOneAssociation_4004 = getElementType(
			"net.codecadenza.eclipse.diagram.ManyToOneAssociation_4004");
	public static final IElementType OneToManyAssociation_4006 = getElementType(
			"net.codecadenza.eclipse.diagram.OneToManyAssociation_4006");
	public static final IElementType EnumAssociation_4001 = getElementType("net.codecadenza.eclipse.diagram.EnumAssociation_4001");
	public static final IElementType DomainInheritance_4005 = getElementType(
			"net.codecadenza.eclipse.diagram.DomainInheritance_4005");

	/**
	 * Prevent instantiation
	 */
	private CodeCadenzaElementTypes() {
	}

	/**
	 * @return the image registry
	 */
	private static synchronized ImageRegistry getImageRegistry() {
		if (imageRegistry == null)
			imageRegistry = new ImageRegistry();

		return imageRegistry;
	}

	/**
	 * @param element
	 * @return the image registry key
	 */
	private static String getImageRegistryKey(ENamedElement element) {
		return element.getName();
	}

	/**
	 * @param element
	 * @return the image descriptor
	 */
	private static ImageDescriptor getProvidedImageDescriptor(ENamedElement element) {
		if (element instanceof final EStructuralFeature feature) {
			final EClass eContainingClass = feature.getEContainingClass();
			final EClassifier eType = feature.getEType();

			if (eContainingClass != null && !eContainingClass.isAbstract())
				element = eContainingClass;
			else if (eType instanceof final EClass eClass && !eClass.isAbstract())
				element = eType;
		}

		if (element instanceof final EClass eClass && !eClass.isAbstract())
			return CodeCadenzaDiagramEditorPlugin.getInstance()
					.getItemImageDescriptor(eClass.getEPackage().getEFactoryInstance().create(eClass));

		return null;
	}

	/**
	 * @param element
	 * @return the image descriptor
	 */
	public static ImageDescriptor getImageDescriptor(ENamedElement element) {
		final String key = getImageRegistryKey(element);
		ImageDescriptor imageDescriptor = getImageRegistry().getDescriptor(key);

		if (imageDescriptor == null) {
			imageDescriptor = getProvidedImageDescriptor(element);

			if (imageDescriptor == null)
				imageDescriptor = ImageDescriptor.getMissingImageDescriptor();

			getImageRegistry().put(key, imageDescriptor);
		}

		return imageDescriptor;
	}

	/**
	 * @param element
	 * @return the image
	 */
	public static Image getImage(ENamedElement element) {
		final String key = getImageRegistryKey(element);
		Image image = getImageRegistry().get(key);

		if (image == null) {
			ImageDescriptor imageDescriptor = getProvidedImageDescriptor(element);

			if (imageDescriptor == null)
				imageDescriptor = ImageDescriptor.getMissingImageDescriptor();

			getImageRegistry().put(key, imageDescriptor);
			image = getImageRegistry().get(key);
		}

		return image;
	}

	/**
	 * @param hint
	 * @return the image descriptor
	 */
	public static ImageDescriptor getImageDescriptor(IAdaptable hint) {
		final ENamedElement element = getElement(hint);

		if (element == null)
			return null;

		return getImageDescriptor(element);
	}

	/**
	 * @param hint
	 * @return the image
	 */
	public static Image getImage(IAdaptable hint) {
		final ENamedElement element = getElement(hint);

		if (element == null)
			return null;

		return getImage(element);
	}

	/**
	 * Get an Ecore object based on the type of the given hint
	 * @param hint
	 * @return a named element
	 */
	public static synchronized ENamedElement getElement(IAdaptable hint) {
		final Object type = hint.getAdapter(IElementType.class);

		if (elements == null) {
			elements = new IdentityHashMap<>();
			elements.put(DomainNamespace_1000, DomainPackage.eINSTANCE.getDomainNamespace());
			elements.put(DomainObject_2001, DomainPackage.eINSTANCE.getDomainObject());
			elements.put(JavaEnum_2002, JavaPackage.eINSTANCE.getJavaEnum());
			elements.put(DomainAttribute_3001, DomainPackage.eINSTANCE.getDomainAttribute());
			elements.put(EnumLiteral_3002, JavaPackage.eINSTANCE.getEnumLiteral());
			elements.put(OneToOneAssociation_4003, DomainPackage.eINSTANCE.getOneToOneAssociation());
			elements.put(ManyToManyAssociation_4002, DomainPackage.eINSTANCE.getManyToManyAssociation());
			elements.put(ManyToOneAssociation_4004, DomainPackage.eINSTANCE.getManyToOneAssociation());
			elements.put(OneToManyAssociation_4006, DomainPackage.eINSTANCE.getOneToManyAssociation());
			elements.put(EnumAssociation_4001, DomainPackage.eINSTANCE.getEnumAssociation());
			elements.put(DomainInheritance_4005, DomainPackage.eINSTANCE.getDomainInheritance());
		}

		return elements.get(type);
	}

	/**
	 * @param id
	 * @return the element type
	 */
	private static IElementType getElementType(String id) {
		return ElementTypeRegistry.getInstance().getType(id);
	}

	/**
	 * @param elementType
	 * @return true if the type is known
	 */
	public static synchronized boolean isKnownElementType(IElementType elementType) {
		if (KNOWN_ELEMENT_TYPES == null) {
			KNOWN_ELEMENT_TYPES = new HashSet<>();
			KNOWN_ELEMENT_TYPES.add(DomainNamespace_1000);
			KNOWN_ELEMENT_TYPES.add(DomainObject_2001);
			KNOWN_ELEMENT_TYPES.add(JavaEnum_2002);
			KNOWN_ELEMENT_TYPES.add(DomainAttribute_3001);
			KNOWN_ELEMENT_TYPES.add(EnumLiteral_3002);
			KNOWN_ELEMENT_TYPES.add(OneToOneAssociation_4003);
			KNOWN_ELEMENT_TYPES.add(ManyToManyAssociation_4002);
			KNOWN_ELEMENT_TYPES.add(ManyToOneAssociation_4004);
			KNOWN_ELEMENT_TYPES.add(OneToManyAssociation_4006);
			KNOWN_ELEMENT_TYPES.add(EnumAssociation_4001);
			KNOWN_ELEMENT_TYPES.add(DomainInheritance_4005);
		}

		return KNOWN_ELEMENT_TYPES.contains(elementType);
	}

}

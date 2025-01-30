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
package net.codecadenza.eclipse.diagram.domain.sheet;

import static net.codecadenza.eclipse.shared.Constants.IMG_ABSTRACT_CLASS;
import static net.codecadenza.eclipse.shared.Constants.IMG_ATTRIBUTE;
import static net.codecadenza.eclipse.shared.Constants.IMG_DOMAIN_OBJECT;
import static net.codecadenza.eclipse.shared.Constants.IMG_ENUM;
import static net.codecadenza.eclipse.shared.Constants.IMG_ENUM_ASSOC;
import static net.codecadenza.eclipse.shared.Constants.IMG_ENUM_LITERAL;
import static net.codecadenza.eclipse.shared.Constants.IMG_INHERITANCE;
import static net.codecadenza.eclipse.shared.Constants.IMG_MTM_ASSOC;
import static net.codecadenza.eclipse.shared.Constants.IMG_MTO_ASSOC;
import static net.codecadenza.eclipse.shared.Constants.IMG_OTM_ASSOC;
import static net.codecadenza.eclipse.shared.Constants.IMG_OTO_ASSOC;

import net.codecadenza.eclipse.diagram.domain.navigator.CodeCadenzaNavigatorGroup;
import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaDiagramEditorPlugin;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainInheritance;
import net.codecadenza.eclipse.model.domain.DomainNamespace;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.EnumAssociation;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.domain.OneToOneAssociation;
import net.codecadenza.eclipse.model.java.EnumLiteral;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.resource.CodeCadenzaResourcePlugin;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.emf.edit.ui.provider.AdapterFactoryLabelProvider;
import org.eclipse.gef.EditPart;
import org.eclipse.gmf.runtime.notation.View;
import org.eclipse.jdt.ui.JavaUI;
import org.eclipse.jface.viewers.DecoratingLabelProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.graphics.Image;

/**
 * <p>
 * Sheet label provider
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CodeCadenzaSheetLabelProvider extends DecoratingLabelProvider {
	/**
	 * Constructor
	 */
	public CodeCadenzaSheetLabelProvider() {
		super(new AdapterFactoryLabelProvider(CodeCadenzaDiagramEditorPlugin.getInstance().getItemProvidersAdapterFactory()), null);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.viewers.DecoratingLabelProvider#getText(java.lang.Object)
	 */
	@Override
	public String getText(Object element) {
		final Object selected = unwrap(element);

		if (selected instanceof final CodeCadenzaNavigatorGroup navigatorGroup)
			return navigatorGroup.getGroupName();
		else if (selected instanceof final DomainObject domainObject)
			return "Domain object " + domainObject.getName();
		else if (selected instanceof final DomainAttribute domainAttribute)
			return "Domain attribute " + domainAttribute.getName();
		else if (selected instanceof final JavaEnum javaEnum)
			return "Enum " + javaEnum.getName();
		else if (selected instanceof final EnumAssociation enumAssoc)
			return "Enum association of " + enumAssoc.getTarget().getName();
		else if (selected instanceof final EnumLiteral enumLiteral)
			return "Enum literal " + enumLiteral.getName();
		else if (selected instanceof final OneToManyAssociation otm)
			return "One-to-many association " + otm.getName();
		else if (selected instanceof final OneToOneAssociation oto)
			return "One-to-one association " + oto.getName();
		else if (selected instanceof final ManyToOneAssociation mto)
			return "Many-to-one association " + mto.getName();
		else if (selected instanceof final ManyToManyAssociation mtm)
			return "Many-to-many association " + mtm.getName();
		else if (selected instanceof final DomainNamespace domainNamespace)
			return "Package " + domainNamespace.getName();
		else if (selected instanceof final DomainInheritance domainInheritance)
			return "Inheritance " + domainInheritance.getSource().getName();

		return "";
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.viewers.DecoratingLabelProvider#getImage(java.lang.Object)
	 */
	@Override
	public Image getImage(Object element) {
		final Object obj = unwrap(element);

		if (obj instanceof final DomainObject domainObject) {
			if (domainObject.isAbstract())
				return CodeCadenzaResourcePlugin.getImage(IMG_ABSTRACT_CLASS);

			return CodeCadenzaResourcePlugin.getImage(IMG_DOMAIN_OBJECT);
		}
		else if (obj instanceof DomainAttribute)
			return CodeCadenzaResourcePlugin.getImage(IMG_ATTRIBUTE);
		else if (obj instanceof JavaEnum)
			return CodeCadenzaResourcePlugin.getImage(IMG_ENUM);
		else if (obj instanceof EnumLiteral)
			return CodeCadenzaResourcePlugin.getImage(IMG_ENUM_LITERAL);
		else if (obj instanceof EnumAssociation)
			return CodeCadenzaResourcePlugin.getImage(IMG_ENUM_ASSOC);
		else if (obj instanceof OneToManyAssociation)
			return CodeCadenzaResourcePlugin.getImage(IMG_OTM_ASSOC);
		else if (obj instanceof OneToOneAssociation)
			return CodeCadenzaResourcePlugin.getImage(IMG_OTO_ASSOC);
		else if (obj instanceof ManyToOneAssociation)
			return CodeCadenzaResourcePlugin.getImage(IMG_MTO_ASSOC);
		else if (obj instanceof ManyToManyAssociation)
			return CodeCadenzaResourcePlugin.getImage(IMG_MTM_ASSOC);
		else if (obj instanceof DomainNamespace)
			return JavaUI.getSharedImages().getImage(org.eclipse.jdt.ui.ISharedImages.IMG_OBJS_PACKAGE);
		else if (obj instanceof DomainInheritance)
			return CodeCadenzaResourcePlugin.getImage(IMG_INHERITANCE);

		return super.getImage(obj);
	}

	/**
	 * @param element
	 * @return the unwrapped object
	 */
	private Object unwrap(Object element) {
		if (element instanceof final IStructuredSelection structuredSelection)
			return unwrap(structuredSelection.getFirstElement());

		if (element instanceof final EditPart editPart)
			return unwrapEditPart(editPart);

		if (element instanceof final IAdaptable adaptable) {
			final View view = adaptable.getAdapter(View.class);

			if (view != null)
				return unwrapView(view);
		}

		return element;
	}

	/**
	 * @param p
	 * @return the unwrapped edit part
	 */
	private Object unwrapEditPart(EditPart p) {
		if (p.getModel() instanceof final View view)
			return unwrapView(view);

		return p.getModel();
	}

	/**
	 * @param view
	 * @return the unwrapped view
	 */
	private Object unwrapView(View view) {
		return view.getElement() == null ? view : view.getElement();
	}

}

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
import net.codecadenza.eclipse.diagram.domain.edit.parts.JavaEnumNameEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.ManyToManyAssociationEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.ManyToOneAssociationEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.OneToManyAssociationEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.OneToOneAssociationEditPart;
import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaDiagramEditorPlugin;
import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaVisualIDRegistry;
import net.codecadenza.eclipse.diagram.domain.providers.CodeCadenzaElementTypes;
import net.codecadenza.eclipse.diagram.domain.providers.CodeCadenzaParserProvider;
import net.codecadenza.eclipse.model.domain.DomainNamespace;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.domain.OneToOneAssociation;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.gmf.runtime.common.ui.services.parser.IParser;
import org.eclipse.gmf.runtime.common.ui.services.parser.ParserOptions;
import org.eclipse.gmf.runtime.common.ui.services.parser.ParserService;
import org.eclipse.gmf.runtime.emf.type.core.IElementType;
import org.eclipse.gmf.runtime.notation.View;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.jface.viewers.ITreePathLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.TreePath;
import org.eclipse.jface.viewers.ViewerLabel;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.navigator.ICommonContentExtensionSite;
import org.eclipse.ui.navigator.ICommonLabelProvider;

/**
 * <p>
 * Navigator label provider
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CodeCadenzaNavigatorLabelProvider extends LabelProvider implements ICommonLabelProvider, ITreePathLabelProvider {
	static {
		CodeCadenzaDiagramEditorPlugin.getInstance().getImageRegistry().put("Navigator?UnknownElement",
				ImageDescriptor.getMissingImageDescriptor());
		CodeCadenzaDiagramEditorPlugin.getInstance().getImageRegistry().put("Navigator?ImageNotFound",
				ImageDescriptor.getMissingImageDescriptor());
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.viewers.ITreePathLabelProvider#updateLabel(org.eclipse.jface.viewers.ViewerLabel,
	 * org.eclipse.jface.viewers.TreePath)
	 */
	@Override
	public void updateLabel(ViewerLabel label, TreePath elementPath) {
		final Object element = elementPath.getLastSegment();

		if (element instanceof final CodeCadenzaNavigatorItem navigatorItem && !isOwnView(navigatorItem.getView()))
			return;

		label.setText(getText(element));
		label.setImage(getImage(element));
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.viewers.LabelProvider#getImage(java.lang.Object)
	 */
	@Override
	public Image getImage(Object element) {
		if (element instanceof final CodeCadenzaNavigatorGroup group)
			return CodeCadenzaDiagramEditorPlugin.getInstance().getBundledImage(group.getIcon());

		if (element instanceof final CodeCadenzaNavigatorItem navigatorItem) {
			if (!isOwnView(navigatorItem.getView()))
				return super.getImage(element);

			return getImage(navigatorItem.getView());
		}

		// Due to the plugin.xml the content will be called only for "own" views
		if (element instanceof final IAdaptable adaptable) {
			final View view = adaptable.getAdapter(View.class);

			if (view != null && isOwnView(view))
				return getImage(view);
		}

		return super.getImage(element);
	}

	/**
	 * @param view
	 * @return the image
	 */
	public Image getImage(View view) {
		switch (CodeCadenzaVisualIDRegistry.getVisualID(view)) {
			case DomainNamespaceEditPart.VISUAL_ID:
				return getImage("Navigator?Diagram?http:///net.codecadenza.eclipse/model/domain.ecore?DomainNamespace",
						CodeCadenzaElementTypes.DomainNamespace_1000);
			case DomainObjectEditPart.VISUAL_ID:
				return getImage("Navigator?TopLevelNode?http:///net.codecadenza.eclipse/model/domain.ecore?DomainObject",
						CodeCadenzaElementTypes.DomainObject_2001);
			case JavaEnumEditPart.VISUAL_ID:
				return getImage("Navigator?TopLevelNode?http:///net.codecadenza.eclipse/model/java.ecore?JavaEnum",
						CodeCadenzaElementTypes.JavaEnum_2002);
			case DomainAttributeEditPart.VISUAL_ID:
				return getImage("Navigator?Node?http:///net.codecadenza.eclipse/model/domain.ecore?DomainAttribute",
						CodeCadenzaElementTypes.DomainAttribute_3001);
			case EnumLiteralEditPart.VISUAL_ID:
				return getImage("Navigator?Node?http:///net.codecadenza.eclipse/model/java.ecore?EnumLiteral",
						CodeCadenzaElementTypes.EnumLiteral_3002);
			case OneToOneAssociationEditPart.VISUAL_ID:
				return getImage("Navigator?Link?http:///net.codecadenza.eclipse/model/domain.ecore?OneToOneAssociation",
						CodeCadenzaElementTypes.OneToOneAssociation_4003);
			case ManyToManyAssociationEditPart.VISUAL_ID:
				return getImage("Navigator?Link?http:///net.codecadenza.eclipse/model/domain.ecore?ManyToManyAssociation",
						CodeCadenzaElementTypes.ManyToManyAssociation_4002);
			case ManyToOneAssociationEditPart.VISUAL_ID:
				return getImage("Navigator?Link?http:///net.codecadenza.eclipse/model/domain.ecore?ManyToOneAssociation",
						CodeCadenzaElementTypes.ManyToOneAssociation_4004);
			case OneToManyAssociationEditPart.VISUAL_ID:
				return getImage("Navigator?Link?http:///net.codecadenza.eclipse/model/domain.ecore?OneToManyAssociation",
						CodeCadenzaElementTypes.OneToManyAssociation_4006);
			case EnumAssociationEditPart.VISUAL_ID:
				return getImage("Navigator?Link?http:///net.codecadenza.eclipse/model/domain.ecore?EnumAssociation",
						CodeCadenzaElementTypes.EnumAssociation_4001);
			case DomainObjectInheritanceEditPart.VISUAL_ID:
				return getImage("Navigator?Link?http:///net.codecadenza.eclipse/model/domain.ecore?DomainInheritance",
						CodeCadenzaElementTypes.DomainInheritance_4005);
		}

		return getImage("Navigator?UnknownElement", null);
	}

	/**
	 * @param key
	 * @param elementType
	 * @return the image
	 */
	private Image getImage(String key, IElementType elementType) {
		final ImageRegistry imageRegistry = CodeCadenzaDiagramEditorPlugin.getInstance().getImageRegistry();
		Image image = imageRegistry.get(key);

		if (image == null && elementType != null && CodeCadenzaElementTypes.isKnownElementType(elementType)) {
			image = CodeCadenzaElementTypes.getImage(elementType);
			imageRegistry.put(key, image);
		}

		if (image == null) {
			image = imageRegistry.get("Navigator?ImageNotFound");
			imageRegistry.put(key, image);
		}

		return image;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
	 */
	@Override
	public String getText(Object element) {
		if (element instanceof final CodeCadenzaNavigatorGroup group)
			return group.getGroupName();

		if (element instanceof final CodeCadenzaNavigatorItem navigatorItem) {
			if (!isOwnView(navigatorItem.getView()))
				return null;

			return getText(navigatorItem.getView());
		}

		// Due to the plugin.xml the content will be called only for "own" views
		if (element instanceof final IAdaptable adaptable) {
			final View view = adaptable.getAdapter(View.class);

			if (view != null && isOwnView(view))
				return getText(view);
		}

		return super.getText(element);
	}

	/**
	 * @param view
	 * @return the text string
	 */
	public String getText(View view) {
		if (view.getElement() != null && view.getElement().eIsProxy())
			return getUnresolvedDomainElementProxyText(view);

		switch (CodeCadenzaVisualIDRegistry.getVisualID(view)) {
			case DomainNamespaceEditPart.VISUAL_ID:
				return getDomainNamespace_1000Text(view);
			case DomainObjectEditPart.VISUAL_ID:
				return getDomainObject_2001Text(view);
			case JavaEnumEditPart.VISUAL_ID:
				return getJavaEnum_2002Text(view);
			case DomainAttributeEditPart.VISUAL_ID:
				return getDomainAttribute_3001Text(view);
			case EnumLiteralEditPart.VISUAL_ID:
				return getEnumLiteral_3002Text(view);
			case OneToOneAssociationEditPart.VISUAL_ID:
				return getOneToOneAssociation_4003Text(view);
			case ManyToManyAssociationEditPart.VISUAL_ID:
				return getManyToManyAssociation_4002Text(view);
			case ManyToOneAssociationEditPart.VISUAL_ID:
				return getManyToOneAssociation_4004Text(view);
			case OneToManyAssociationEditPart.VISUAL_ID:
				return getOneToManyAssociation_4006Text(view);
			case EnumAssociationEditPart.VISUAL_ID:
				return "";
			case DomainObjectInheritanceEditPart.VISUAL_ID:
				return "";
		}

		return getUnknownElementText(view);
	}

	/**
	 * @param view
	 * @return the text string
	 */
	private String getDomainNamespace_1000Text(View view) {
		final var domainModelElement = (DomainNamespace) view.getElement();

		if (domainModelElement != null)
			return domainModelElement.getName();

		CodeCadenzaDiagramEditorPlugin.getInstance().logError("No domain element for view with visualID = " + 1000, null);
		return "";
	}

	/**
	 * @param view
	 * @return the text string
	 */
	private String getDomainObject_2001Text(View view) {
		final var hintAdapter = new CodeCadenzaParserProvider.HintAdapter(CodeCadenzaElementTypes.DomainObject_2001,
				(view.getElement() != null ? view.getElement() : view),
				CodeCadenzaVisualIDRegistry.getType(DomainObjectNameEditPart.VISUAL_ID));
		final IParser parser = ParserService.getInstance().getParser(hintAdapter);

		if (parser != null)
			return parser.getPrintString(hintAdapter, ParserOptions.NONE.intValue());

		CodeCadenzaDiagramEditorPlugin.getInstance().logError("Parser was not found for label " + 5002, null);
		return "";
	}

	/**
	 * @param view
	 * @return the text string
	 */
	private String getJavaEnum_2002Text(View view) {
		final var hintAdapter = new CodeCadenzaParserProvider.HintAdapter(CodeCadenzaElementTypes.JavaEnum_2002,
				(view.getElement() != null ? view.getElement() : view),
				CodeCadenzaVisualIDRegistry.getType(JavaEnumNameEditPart.VISUAL_ID));
		final IParser parser = ParserService.getInstance().getParser(hintAdapter);

		if (parser != null)
			return parser.getPrintString(hintAdapter, ParserOptions.NONE.intValue());

		CodeCadenzaDiagramEditorPlugin.getInstance().logError("Parser was not found for label " + 5004, null);
		return "";
	}

	/**
	 * @param view
	 * @return the text string
	 */
	private String getDomainAttribute_3001Text(View view) {
		final var hintAdapter = new CodeCadenzaParserProvider.HintAdapter(CodeCadenzaElementTypes.DomainAttribute_3001,
				(view.getElement() != null ? view.getElement() : view),
				CodeCadenzaVisualIDRegistry.getType(DomainAttributeNameEditPart.VISUAL_ID));
		final IParser parser = ParserService.getInstance().getParser(hintAdapter);

		if (parser != null)
			return parser.getPrintString(hintAdapter, ParserOptions.NONE.intValue());

		CodeCadenzaDiagramEditorPlugin.getInstance().logError("Parser was not found for label " + 5001, null);
		return "";
	}

	/**
	 * @param view
	 * @return the text string
	 */
	private String getEnumLiteral_3002Text(View view) {
		final var hintAdapter = new CodeCadenzaParserProvider.HintAdapter(CodeCadenzaElementTypes.EnumLiteral_3002,
				(view.getElement() != null ? view.getElement() : view),
				CodeCadenzaVisualIDRegistry.getType(EnumLiteralNameEditPart.VISUAL_ID));
		final IParser parser = ParserService.getInstance().getParser(hintAdapter);

		if (parser != null)
			return parser.getPrintString(hintAdapter, ParserOptions.NONE.intValue());

		CodeCadenzaDiagramEditorPlugin.getInstance().logError("Parser was not found for label " + 5003, null);
		return "";
	}

	/**
	 * @param view
	 * @return the text string
	 */
	private String getOneToOneAssociation_4003Text(View view) {
		final var domainModelElement = (OneToOneAssociation) view.getElement();

		if (domainModelElement != null)
			return domainModelElement.getName();

		CodeCadenzaDiagramEditorPlugin.getInstance().logError("No domain element for view with visualID = " + 4003, null);
		return "";
	}

	/**
	 * @param view
	 * @return the text string
	 */
	private String getManyToManyAssociation_4002Text(View view) {
		final var domainModelElement = (ManyToManyAssociation) view.getElement();

		if (domainModelElement != null)
			return domainModelElement.getName();

		CodeCadenzaDiagramEditorPlugin.getInstance().logError("No domain element for view with visualID = " + 4002, null);
		return "";
	}

	/**
	 * @param view
	 * @return the text string
	 */
	private String getManyToOneAssociation_4004Text(View view) {
		final var domainModelElement = (ManyToOneAssociation) view.getElement();

		if (domainModelElement != null)
			return domainModelElement.getName();

		CodeCadenzaDiagramEditorPlugin.getInstance().logError("No domain element for view with visualID = " + 4004, null);
		return "";
	}

	/**
	 * @param view
	 * @return the text string
	 */
	private String getOneToManyAssociation_4006Text(View view) {
		final var domainModelElement = (OneToManyAssociation) view.getElement();

		if (domainModelElement != null)
			return domainModelElement.getName();

		CodeCadenzaDiagramEditorPlugin.getInstance().logError("No domain element for view with visualID = " + 4006, null);
		return "";
	}

	/**
	 * @param view
	 * @return the text string
	 */
	private String getUnknownElementText(View view) {
		return "<UnknownElement Visual_ID = " + view.getType() + ">";
	}

	/**
	 * @param view
	 * @return the text string
	 */
	private String getUnresolvedDomainElementProxyText(View view) {
		return "<Unresolved domain element Visual_ID = " + view.getType() + ">";
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.navigator.ICommonLabelProvider#init(org.eclipse.ui.navigator.ICommonContentExtensionSite)
	 */
	@Override
	public void init(ICommonContentExtensionSite aConfig) {
		// No implementation required!
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
	 * @see org.eclipse.ui.navigator.IDescriptionProvider#getDescription(java.lang.Object)
	 */
	@Override
	public String getDescription(Object anElement) {
		return null;
	}

	/**
	 * @param view
	 * @return true if the given view belongs to this plug-in
	 */
	private boolean isOwnView(View view) {
		return DomainNamespaceEditPart.MODEL_ID.equals(CodeCadenzaVisualIDRegistry.getModelID(view));
	}

}

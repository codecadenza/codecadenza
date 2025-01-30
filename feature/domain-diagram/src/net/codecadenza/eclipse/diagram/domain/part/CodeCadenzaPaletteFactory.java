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

import java.util.ArrayList;
import java.util.List;
import net.codecadenza.eclipse.diagram.domain.providers.CodeCadenzaElementTypes;
import net.codecadenza.eclipse.resource.CodeCadenzaResourcePlugin;
import org.eclipse.gef.Tool;
import org.eclipse.gef.palette.PaletteContainer;
import org.eclipse.gef.palette.PaletteDrawer;
import org.eclipse.gef.palette.PaletteRoot;
import org.eclipse.gef.palette.ToolEntry;
import org.eclipse.gmf.runtime.diagram.ui.tools.UnspecifiedTypeConnectionTool;
import org.eclipse.gmf.runtime.diagram.ui.tools.UnspecifiedTypeCreationTool;
import org.eclipse.gmf.runtime.emf.type.core.IElementType;

/**
 * <p>
 * Palette factory
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CodeCadenzaPaletteFactory {
	/**
	 * @param paletteRoot
	 */
	public void fillPalette(PaletteRoot paletteRoot) {
		paletteRoot.add(createDomainobjects1Group());
		paletteRoot.add(createAssociations2Group());
	}

	/**
	 * Create the "Domain objects" palette tool group
	 * @return the palette container
	 */
	private PaletteContainer createDomainobjects1Group() {
		final var paletteContainer = new PaletteDrawer(Messages.Domainobjects1Group_title);
		paletteContainer.setDescription(Messages.Domainobjects1Group_desc);
		paletteContainer.add(createDomainObjectCreationTool());
		paletteContainer.add(createDomainAttributeCreationTool());
		paletteContainer.add(createDomainInheritanceCreationTool());
		paletteContainer.add(createEnumerationCreationTool());
		paletteContainer.add(createEnumerationLiteral5CreationTool());

		return paletteContainer;
	}

	/**
	 * Create the "Associations" palette tool group
	 * @return the palette container
	 */
	private PaletteContainer createAssociations2Group() {
		final var paletteContainer = new PaletteDrawer(Messages.Associations2Group_title);
		paletteContainer.setDescription(Messages.Associations2Group_desc);
		paletteContainer.add(createOneToOneAssociation1CreationTool());
		paletteContainer.add(createOneToManyAssociation2CreationTool());
		paletteContainer.add(createManyToOneAssociation3CreationTool());
		paletteContainer.add(createManyToManyAssociation4CreationTool());
		paletteContainer.add(createEnumAssociation5CreationTool());

		return paletteContainer;
	}

	/**
	 * @return the tool entry
	 */
	private ToolEntry createDomainObjectCreationTool() {
		final var types = new ArrayList<IElementType>(1);
		types.add(CodeCadenzaElementTypes.DomainObject_2001);

		final var entry = new NodeToolEntry(Messages.DomainObject1CreationTool_title, Messages.DomainObject1CreationTool_desc, types);
		entry.setSmallIcon(CodeCadenzaResourcePlugin.getImageDescriptor(IMG_DOMAIN_OBJECT));
		entry.setLargeIcon(entry.getSmallIcon());

		return entry;
	}

	/**
	 * @return the tool entry
	 */
	private ToolEntry createDomainAttributeCreationTool() {
		final var types = new ArrayList<IElementType>(1);
		types.add(CodeCadenzaElementTypes.DomainAttribute_3001);

		final var entry = new NodeToolEntry(Messages.DomainAttribute2CreationTool_title, Messages.DomainAttribute2CreationTool_desc,
				types);
		entry.setSmallIcon(CodeCadenzaResourcePlugin.getImageDescriptor(IMG_ATTRIBUTE));
		entry.setLargeIcon(entry.getSmallIcon());

		return entry;
	}

	/**
	 * @return the tool entry
	 */
	private ToolEntry createDomainInheritanceCreationTool() {
		final var types = new ArrayList<IElementType>(1);
		types.add(CodeCadenzaElementTypes.DomainInheritance_4005);

		final var entry = new LinkToolEntry(Messages.DomainInheritance3CreationTool_title,
				Messages.DomainInheritance3CreationTool_desc, types);
		entry.setSmallIcon(CodeCadenzaResourcePlugin.getImageDescriptor(IMG_INHERITANCE));
		entry.setLargeIcon(entry.getSmallIcon());

		return entry;
	}

	/**
	 * @return the tool entry
	 */
	private ToolEntry createEnumerationCreationTool() {
		final var types = new ArrayList<IElementType>(1);
		types.add(CodeCadenzaElementTypes.JavaEnum_2002);

		final var entry = new NodeToolEntry(Messages.Enumeration4CreationTool_title, Messages.Enumeration4CreationTool_desc, types);
		entry.setSmallIcon(CodeCadenzaResourcePlugin.getImageDescriptor(IMG_ENUM));
		entry.setLargeIcon(entry.getSmallIcon());

		return entry;
	}

	/**
	 * @return the tool entry
	 */
	private ToolEntry createEnumerationLiteral5CreationTool() {
		final var types = new ArrayList<IElementType>(1);
		types.add(CodeCadenzaElementTypes.EnumLiteral_3002);

		final var entry = new NodeToolEntry(Messages.EnumerationLiteral5CreationTool_title,
				Messages.EnumerationLiteral5CreationTool_desc, types);
		entry.setSmallIcon(CodeCadenzaResourcePlugin.getImageDescriptor(IMG_ENUM_LITERAL));
		entry.setLargeIcon(entry.getSmallIcon());

		return entry;
	}

	/**
	 * @return the tool entry
	 */
	private ToolEntry createOneToOneAssociation1CreationTool() {
		final var types = new ArrayList<IElementType>(1);
		types.add(CodeCadenzaElementTypes.OneToOneAssociation_4003);

		final var entry = new LinkToolEntry(Messages.OneToOneAssociation1CreationTool_title,
				Messages.OneToOneAssociation1CreationTool_desc, types);
		entry.setSmallIcon(CodeCadenzaResourcePlugin.getImageDescriptor(IMG_OTO_ASSOC));
		entry.setLargeIcon(entry.getSmallIcon());

		return entry;
	}

	/**
	 * @return the tool entry
	 */
	private ToolEntry createOneToManyAssociation2CreationTool() {
		final var types = new ArrayList<IElementType>(1);
		types.add(CodeCadenzaElementTypes.OneToManyAssociation_4006);

		final var entry = new LinkToolEntry(Messages.OneToManyAssociation2CreationTool_title,
				Messages.OneToManyAssociation2CreationTool_desc, types);
		entry.setSmallIcon(CodeCadenzaResourcePlugin.getImageDescriptor(IMG_OTM_ASSOC));
		entry.setLargeIcon(entry.getSmallIcon());

		return entry;
	}

	/**
	 * @return the tool entry
	 */
	private ToolEntry createManyToOneAssociation3CreationTool() {
		final var types = new ArrayList<IElementType>(1);
		types.add(CodeCadenzaElementTypes.ManyToOneAssociation_4004);

		final var entry = new LinkToolEntry(Messages.ManyToOneAssociation3CreationTool_title,
				Messages.ManyToOneAssociation3CreationTool_desc, types);
		entry.setSmallIcon(CodeCadenzaResourcePlugin.getImageDescriptor(IMG_MTO_ASSOC));
		entry.setLargeIcon(entry.getSmallIcon());

		return entry;
	}

	/**
	 * @return the tool entry
	 */
	private ToolEntry createManyToManyAssociation4CreationTool() {
		final var types = new ArrayList<IElementType>(1);
		types.add(CodeCadenzaElementTypes.ManyToManyAssociation_4002);

		final var entry = new LinkToolEntry(Messages.ManyToManyAssociation4CreationTool_title,
				Messages.ManyToManyAssociation4CreationTool_desc, types);
		entry.setSmallIcon(CodeCadenzaResourcePlugin.getImageDescriptor(IMG_MTM_ASSOC));
		entry.setLargeIcon(entry.getSmallIcon());

		return entry;
	}

	/**
	 * @return the tool entry
	 */
	private ToolEntry createEnumAssociation5CreationTool() {
		final var types = new ArrayList<IElementType>(1);
		types.add(CodeCadenzaElementTypes.EnumAssociation_4001);

		final var entry = new LinkToolEntry(Messages.EnumAssociation5CreationTool_title, Messages.EnumAssociation5CreationTool_desc,
				types);
		entry.setSmallIcon(CodeCadenzaResourcePlugin.getImageDescriptor(IMG_ENUM_ASSOC));
		entry.setLargeIcon(entry.getSmallIcon());

		return entry;
	}

	private static class NodeToolEntry extends ToolEntry {
		private final List<IElementType> elementTypes;

		/**
		 * @param title
		 * @param description
		 * @param elementTypes
		 */
		private NodeToolEntry(String title, String description, List<IElementType> elementTypes) {
			super(title, description, null, null);

			this.elementTypes = elementTypes;
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.gef.palette.ToolEntry#createTool()
		 */
		@Override
		public Tool createTool() {
			final var tool = new UnspecifiedTypeCreationTool(elementTypes);
			tool.setProperties(getToolProperties());

			return tool;
		}
	}

	private static class LinkToolEntry extends ToolEntry {
		private final List<IElementType> relationshipTypes;

		/**
		 * @param title
		 * @param description
		 * @param relationshipTypes
		 */
		private LinkToolEntry(String title, String description, List<IElementType> relationshipTypes) {
			super(title, description, null, null);

			this.relationshipTypes = relationshipTypes;
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.gef.palette.ToolEntry#createTool()
		 */
		@Override
		public Tool createTool() {
			final var tool = new UnspecifiedTypeConnectionTool(relationshipTypes);
			tool.setProperties(getToolProperties());

			return tool;
		}
	}

}

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
package net.codecadenza.eclipse.generator.client.imp.angular.domain;

import static net.codecadenza.eclipse.generator.client.imp.angular.common.JavaScriptType.ARRAY;
import static net.codecadenza.eclipse.generator.client.imp.angular.common.JavaScriptType.BOOLEAN;
import static net.codecadenza.eclipse.generator.client.imp.angular.common.JavaScriptType.DATE;
import static net.codecadenza.eclipse.generator.client.imp.angular.common.JavaScriptType.NUMBER;
import static net.codecadenza.eclipse.generator.client.imp.angular.common.JavaScriptType.STRING;

import net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.common.AngularContentFormatter;
import net.codecadenza.eclipse.generator.client.imp.angular.common.TypeScriptFieldGenerator;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.java.JavaType;

/**
 * <p>
 * Generator for domain objects that are used in an Angular application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class AngularDomainObjectGenerator extends AbstractTypeScriptSourceGenerator {
	private final DTOBean dto;

	/**
	 * Constructor
	 * @param dto
	 */
	public AngularDomainObjectGenerator(DTOBean dto) {
		super(dto.getTypeScriptSourceFile(), dto.getComment());

		this.dto = dto;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		// Add imports for referenced types
		dto.getAttributes().stream().forEach(attr -> {
			if (attr.getDomainAttribute() == null) {
				final String typeName = attr.getReferencedDTOBean().getName();

				if (!dto.getName().equals(attr.getReferencedDTOBean().getName()))
					importType(typeName, "./" + typeName.toLowerCase() + ".interface");
			}
			else if (attr.getDomainAttribute().getJavaType().isEnum()) {
				final String typeName = attr.getDomainAttribute().getJavaType().getName();

				importType(typeName, "./" + typeName.toLowerCase() + ".enum");
			}
		});
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator#
	 * addTypeDeclaration(net.codecadenza.eclipse.generator.client.imp.angular.common.AngularContentFormatter)
	 */
	@Override
	protected void addTypeDeclaration(AngularContentFormatter formatter) {
		formatter.addLine("export interface " + dto.getName() + " {");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		dto.getAttributes().forEach(attr -> {
			final TypeScriptFieldGenerator fieldGenerator;

			if (attr.getDomainAttribute() != null) {
				final JavaType type = attr.getDomainAttribute().getJavaType();

				if (type.isBoolean())
					fieldGenerator = addField(BOOLEAN, attr.getName());
				else if (type.isNumber())
					fieldGenerator = addField(NUMBER, attr.getName());
				else if (type.isTemporalType())
					fieldGenerator = addField(DATE, attr.getName());
				else if (type.isEnum())
					fieldGenerator = addField(type.getName(), attr.getName());
				else
					fieldGenerator = addField(STRING, attr.getName());
			}
			else {
				final AbstractDomainAssociation assoc = attr.getAssociation();
				String typeName = attr.getReferencedDTOBean().getName();

				if (assoc instanceof final ManyToOneAssociation mto && mto.isOptional())
					typeName += " | null";
				else if (assoc instanceof ManyToManyAssociation)
					typeName = ARRAY + "<" + attr.getReferencedDTOBean().getName() + ">";

				fieldGenerator = addField(typeName, attr.getName());
			}

			fieldGenerator.create();
		});
	}

}

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
package net.codecadenza.eclipse.generator.boundary.method;

import java.util.HashSet;
import java.util.Set;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.common.IMethodGeneratorUtility;
import net.codecadenza.eclipse.generator.dto.DTOInlineConversionGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.dto.DTOBean;

/**
 * <p>
 * Generator for boundary methods to get a list containing all existing objects
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class FindAllBoundaryMethodGenerator extends BasicBoundaryMethodGenerator {
	/**
	 * Constructor
	 * @param method
	 * @param utility
	 * @param parentGenerator
	 */
	public FindAllBoundaryMethodGenerator(BoundaryMethod method, IMethodGeneratorUtility utility,
			AbstractJavaSourceGenerator parentGenerator) {
		super(method, utility, parentGenerator);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.BasicBoundaryMethodGenerator#createComment()
	 */
	@Override
	public String createComment() {
		final var b = new StringBuilder();
		b.append(method.generateBeginOfJavadocComment());
		b.append(" * @return a list of " + domainObjectPluralLabel + " objects\n");
		b.append(" */\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.BasicBoundaryMethodGenerator#getImports()
	 */
	@Override
	public Set<String> getImports() {
		final var converter = new DTOInlineConversionGenerator(BoundaryMethodTypeEnumeration.FIND_BY_ID,
				(DTOBean) method.getReturnType(), "name", "dto");

		final Set<String> imports = new HashSet<>(super.getImports());
		imports.addAll(converter.getReverseImports());

		return imports;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.BasicBoundaryMethodGenerator#createMethodBody()
	 */
	@Override
	public String createMethodBody() {
		final var b = new StringBuilder();
		final var dtoBean = (DTOBean) method.getReturnType();
		final String domainObjName = dtoBean.getDomainObject().getLowerCaseName();
		final var converter = new DTOInlineConversionGenerator(method.getMethodType(), dtoBean, domainObjName, "dto");

		b.append("final var resultList = new ArrayList<" + dtoBean.getName() + ">();\n\n");
		b.append("for(final " + dtoBean.getDomainObject().getName() + " " + domainObjName + " : ");
		b.append(getRepositoryName() + method.getServiceMethod().getName() + "())\n");
		b.append("{\n");
		b.append("final var dto = new " + dtoBean.getName() + "();\n");
		b.append(converter.addReverseAttributeSetters());
		b.append("\nresultList.add(dto);\n");
		b.append("}\n\n");
		b.append("return resultList;\n");

		return b.toString();
	}

}

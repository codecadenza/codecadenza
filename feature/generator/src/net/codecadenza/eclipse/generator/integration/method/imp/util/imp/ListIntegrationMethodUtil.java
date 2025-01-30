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
package net.codecadenza.eclipse.generator.integration.method.imp.util.imp;

import static net.codecadenza.eclipse.shared.Constants.DEFAULT_REPOSITORY;

import java.util.HashSet;
import java.util.Set;
import net.codecadenza.eclipse.generator.dto.DTOInlineConversionGenerator;
import net.codecadenza.eclipse.generator.integration.method.imp.util.AbstractIntegrationMethodUtil;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod;

/**
 * <p>
 * Generator utility for facade methods that convert a list of domain objects into respective data transfer objects
 * </p>
 * <p>
 * Copyright 2016 (C) Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ListIntegrationMethodUtil extends AbstractIntegrationMethodUtil {
	/**
	 * Constructor
	 * @param method
	 */
	public ListIntegrationMethodUtil(AbstractIntegrationMethod method) {
		super(method);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.integration.method.imp.util.AbstractIntegrationMethodUtil#createConversionFragment()
	 */
	@Override
	public String createConversionFragment() {
		final var b = new StringBuilder();
		final var dtoBean = (DTOBean) method.getReturnType();
		final String domainObjName = dtoBean.getDomainObject().getLowerCaseName();
		final var converter = new DTOInlineConversionGenerator(type, dtoBean, domainObjName, OBJ_INSTANCE_NAME);

		b.append("final var " + LIST_INSTANCE_NAME + " = new ArrayList<" + dtoBean.getName() + ">();\n");
		b.append("final List<" + dtoBean.getDomainObject().getName() + "> items = ");
		b.append(createServiceMethodInvocation(method, DEFAULT_REPOSITORY) + "\n");
		b.append("for(final " + dtoBean.getDomainObject().getName() + " " + domainObjName + " : items)\n");
		b.append("{\n");
		b.append("final var " + OBJ_INSTANCE_NAME + " = new " + dtoBean.getName() + "();\n");
		b.append(converter.addReverseAttributeSetters());
		b.append("\n");
		b.append(LIST_INSTANCE_NAME + ".add(" + OBJ_INSTANCE_NAME + ");\n");
		b.append("}\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.integration.method.imp.util.AbstractIntegrationMethodUtil#getImports()
	 */
	@Override
	public Set<String> getImports() {
		final var imports = new HashSet<String>();
		final var dtoBean = (DTOBean) method.getReturnType();
		final var converter = new DTOInlineConversionGenerator(type, dtoBean, "", "");

		imports.addAll(super.getImports());
		imports.addAll(converter.getReverseImports());
		imports.add("import " + dtoBean.getDomainObject().getNamespace().toString() + ".*;");

		return imports;
	}

}

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

import static net.codecadenza.eclipse.shared.Constants.REPO_METHOD_NAME_SEARCH;

import java.util.HashSet;
import java.util.Set;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.common.IMethodGeneratorUtility;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodDataFetchType;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;

/**
 * <p>
 * Generator for boundary methods to search for objects
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SearchBoundaryMethodGenerator extends BasicBoundaryMethodGenerator {
	/**
	 * Constructor
	 * @param method
	 * @param utility
	 * @param parentGenerator
	 */
	public SearchBoundaryMethodGenerator(BoundaryMethod method, IMethodGeneratorUtility utility,
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
		b.append(" * @param searchObj a generic container that holds filter criteria\n");

		if (clientParamComment != null)
			b.append(clientParamComment);

		b.append(" * @return a list of " + domainObjectLabel + " objects\n");
		b.append(" * @throws GeneralSearchException if the search operation has failed\n");
		b.append(" */\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.BasicBoundaryMethodGenerator#createMethodBody()
	 */
	@Override
	public String createMethodBody() {
		final var b = new StringBuilder();
		final var dto = (DTOBean) method.getReturnType();

		// Extract the from clause from the select statement
		var fromClause = method.getQueryStatement().trim() + " ";
		final String customStatement = replaceCRLFCharacters(method.getCustomStatement());
		var groupBy = "";

		if (customStatement.contains("group by")) {
			groupBy = customStatement.substring(customStatement.indexOf("group by"));
			fromClause += customStatement.substring(0, customStatement.indexOf("group by"));
		}
		else
			fromClause += customStatement;

		b.append("// Collect the select tokens of all fields that should be fetched\n");
		b.append("final var selectTokens = new ArrayList<String>();\n");

		for (final DTOBeanAttribute attr : dto.getAttributes()) {
			if (attr.getSelectToken() == null)
				continue;

			b.append("selectTokens.add(" + attr.getSelectTokenConstant() + ");\n");
		}

		b.append("\n");
		b.append("searchObj.setFromClause(\"" + fromClause.trim() + "\");\n\n");

		if (!groupBy.isEmpty())
			b.append("searchObj.setGroupBy(\"" + groupBy.trim() + "\");\n\n");

		if (method.getDataFetchType() != BoundaryMethodDataFetchType.DEFAULT) {
			b.append(addAdditionalSearchField(null));
			b.append("\n");
		}

		if (method.getDataFetchType() != BoundaryMethodDataFetchType.DEFAULT)
			b.append("final " + method.getReturnTypeModifier().toString() + "<" + method.getReturnType().getName() + "> resultList = ");
		else
			b.append("return ");

		b.append(getRepositoryName() + REPO_METHOD_NAME_SEARCH + "(searchObj, ");
		b.append(method.getReturnType().getName() + ".class, selectTokens);\n");

		// We must remove the client/user filter field to avoid runtime problems in the client!
		if (method.getDataFetchType() != BoundaryMethodDataFetchType.DEFAULT) {
			b.append("\n");
			b.append("searchObj.getSearchFields().remove(clientFilterField);\n\n");
			b.append("return resultList;\n");
		}

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.BasicBoundaryMethodGenerator#getImports()
	 */
	@Override
	public Set<String> getImports() {
		final var imports = new HashSet<>(super.getImports());
		imports.add("import net.codecadenza.runtime.search.dto.*;");
		imports.add("import net.codecadenza.runtime.search.exception.*;");

		return imports;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.BasicBoundaryMethodGenerator#getInterfaceImports()
	 */
	@Override
	public Set<String> getInterfaceImports() {
		final var imports = new HashSet<>(super.getInterfaceImports());
		imports.add("import net.codecadenza.runtime.search.dto.*;");

		return imports;
	}

}

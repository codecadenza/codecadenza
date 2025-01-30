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
package net.codecadenza.eclipse.generator.facade.method;

import static net.codecadenza.eclipse.shared.Constants.REPO_METHOD_NAME_SEARCH;

import net.codecadenza.eclipse.generator.boundary.method.SearchByFilterBoundaryMethodGenerator;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.common.IMethodGeneratorUtility;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;

/**
 * <p>
 * Generator for facade methods that perform queries using a single filter
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SearchByFilterFacadeMethodGenerator extends SearchByFilterBoundaryMethodGenerator {
	/**
	 * Constructor
	 * @param method
	 * @param utility
	 * @param parentGenerator
	 */
	public SearchByFilterFacadeMethodGenerator(BoundaryMethod method, IMethodGeneratorUtility utility,
			AbstractJavaSourceGenerator parentGenerator) {
		super(method, utility, parentGenerator);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.GetSimpleListBoundaryMethodGenerator#createMethodBody()
	 */
	@Override
	public String createMethodBody() {
		final var b = new StringBuilder();
		final var dto = (DTOBean) method.getReturnType();
		var fromClause = method.getQueryStatement().trim() + " ";
		final String customStatement = replaceCRLFCharacters(method.getCustomStatement());
		var groupBy = "";
		final String returnType = dto.getDomainObject().getName();
		DTOBeanAttribute filterAttribute = null;

		if (dto.getDisplayAttribute() == null)
			filterAttribute = dto.getPKAttribute();
		else
			filterAttribute = dto.getDisplayAttribute();

		if (customStatement.contains("group by")) {
			groupBy = customStatement.substring(customStatement.indexOf("group by"));
			fromClause += customStatement.substring(0, customStatement.indexOf("group by"));
		}
		else
			fromClause += customStatement;

		if (!filterAttribute.getDomainAttribute().isWildcardFilteringSupported()) {
			b.append("if(filter != null && !filter.isEmpty() && !filter.equals(WILDCARD))\n");
			b.append("try\n");
			b.append("{\n");
			b.append(filterAttribute.getDomainAttribute().convertFromString("filter") + ";\n");
			b.append("}\n");

			if (filterAttribute.getDomainAttribute().getJavaType().isUUID())
				b.append("catch(IllegalArgumentException e)\n");
			else
				b.append("catch(NumberFormatException e)\n");

			b.append("{\n");
			b.append("return Collections.emptyList();\n");
			b.append("}\n\n");
		}

		b.append("// Initialize search object\n");
		b.append("final var searchObj = new SearchDTO();\n");
		b.append("searchObj.setExactFilterMatch(true);\n");
		b.append("searchObj.setCaseSensitive(true);\n");
		b.append("searchObj.setMaxResult(SMALL_LIST_SIZE);\n\n");
		b.append("final var statementBuilder = new StringBuilder(\"select a \");\n");
		b.append("statementBuilder.append(\"" + fromClause + "\");\n\n");
		b.append("searchObj.setFromClause(statementBuilder.toString());\n");

		if (!groupBy.isEmpty())
			b.append("searchObj.setGroupBy(\"" + groupBy.trim() + "\");\n\n");
		else
			b.append("\n");

		if (filterAttribute.getSelectToken() != null) {
			b.append("if(filter != null && !filter.isEmpty() && !filter.equals(WILDCARD))\n");
			b.append("{\n");
			b.append("final var filterField = searchObj.addSearchField(\"" + filterAttribute.getSelectToken() + "\", ");
			b.append(filterAttribute.getDomainAttribute().getSearchFieldDataType());
			b.append(");\n");
			b.append("filterField.setFilterCriteria(filter");

			if (filterAttribute.getDomainAttribute().isWildcardFilteringSupported())
				b.append(" + WILDCARD");

			b.append(");\n");
			b.append("filterField.setSortIndex(1);\n");
			b.append("filterField.setSortOrder(SortDirectionEnum.ASC);\n");
			b.append("}\n\n");

			if (filterAttribute.getDomainAttribute().getDomainObject().isMandated()) {
				b.append(addAdditionalSearchField(filterAttribute));
				b.append("\n");
			}
		}

		b.append("return " + REPO_METHOD_NAME_SEARCH + "(searchObj, " + returnType + ".class);\n");

		return b.toString();
	}

}

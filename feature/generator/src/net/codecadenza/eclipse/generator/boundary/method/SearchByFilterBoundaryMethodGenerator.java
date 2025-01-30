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
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainTagEnumeration;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration;

/**
 * <p>
 * Generator for boundary methods that perform queries using a single filter
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SearchByFilterBoundaryMethodGenerator extends BasicBoundaryMethodGenerator {
	/**
	 * Constructor
	 * @param method
	 * @param utility
	 * @param parentGenerator
	 */
	public SearchByFilterBoundaryMethodGenerator(BoundaryMethod method, IMethodGeneratorUtility utility,
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
		b.append(method.generateBeginOfJavadocComment(true));

		if (method.getBoundaryBean().getDomainObject().isMandated()) {
			final DomainAttribute clientPkAttr = project.getDomainObjectByTag(DomainTagEnumeration.CLIENT).getPKAttribute();
			final String clientParamName = project.getDomainObjectByTag(DomainTagEnumeration.CLIENT).getLowerCaseName()
					+ clientPkAttr.getUpperCaseName();

			b.append(" * @param " + clientParamName + "\n");
		}

		if (method.getReturnTypeModifier() != JavaTypeModifierEnumeration.NONE)
			b.append(" * @return a list of " + domainObjectLabel + " objects\n");
		else
			b.append(" * @return a " + domainObjectLabel + " object\n");

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
		final var dto = (DTOBean) method.getReturnType();
		final DTOBeanAttribute filterAttribute;

		if (dto.getDisplayAttribute() == null)
			filterAttribute = dto.getPKAttribute();
		else
			filterAttribute = dto.getDisplayAttribute();

		return createListMethod(filterAttribute, null);
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

		if (project.isBoundaryMode()) {
			imports.add("import static net.codecadenza.runtime.jpa.AbstractRepository.SMALL_LIST_SIZE;");
			imports.add("import static net.codecadenza.runtime.jpa.AbstractRepository.WILDCARD;");
		}

		return imports;
	}

}

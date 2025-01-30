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
package net.codecadenza.eclipse.generator.integration.method.imp.soap;

import static net.codecadenza.eclipse.generator.integration.method.imp.util.AbstractIntegrationMethodUtil.LIST_INSTANCE_NAME;
import static net.codecadenza.eclipse.shared.Constants.SEARCH_PARAM_NAME;

import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.integration.IntegrationMethodParameter;
import net.codecadenza.eclipse.model.integration.SOAPIntegrationMethod;

/**
 * <p>
 * Generator for search methods
 * </p>
 * <p>
 * Copyright 2016 (C) Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SearchSOAPMethodGenerator extends BasicSOAPMethodGenerator {
	/**
	 * Constructor
	 * @param soapMethod
	 * @param parentGenerator
	 */
	public SearchSOAPMethodGenerator(SOAPIntegrationMethod soapMethod, AbstractJavaSourceGenerator parentGenerator) {
		super(soapMethod, parentGenerator);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.integration.method.imp.soap.BasicSOAPMethodGenerator#createMethodLogic()
	 */
	@Override
	public String createMethodLogic() {
		final var b = new StringBuilder();
		final var dto = (DTOBean) soapMethod.getReturnType();
		boolean firstParam = true;

		b.append(createSearchObjectConversion(dto));
		b.append("final List<" + dto.getName() + "> " + LIST_INSTANCE_NAME + " = ");
		b.append(getServiceName() + "." + soapMethod.getBoundaryMethod().getName() + "(");

		for (final IntegrationMethodParameter param : soapMethod.getIntegrationParameters()) {
			if (firstParam)
				firstParam = false;
			else
				b.append(", ");

			if (param.getName().equals(SEARCH_PARAM_NAME))
				b.append("converter.convert()");
			else
				b.append(param.getName());
		}

		b.append(");\n\n");
		b.append("return " + LIST_INSTANCE_NAME + ".stream().toArray(size -> new ");
		b.append(soapMethod.getReturnType().getName() + "[size]);\n");

		return b.toString();
	}

}

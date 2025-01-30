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
package net.codecadenza.runtime.server.security;

import jakarta.security.enterprise.identitystore.PasswordHash;
import java.lang.invoke.MethodHandles;
import net.codecadenza.runtime.crypto.HashGenerator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Implementation of the {@link PasswordHash} interface that uses the SHA256 algorithm
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SHA256PasswordHash implements PasswordHash {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	/*
	 * (non-Javadoc)
	 * @see jakarta.security.enterprise.identitystore.PasswordHash#generate(char[])
	 */
	@Override
	public String generate(char[] password) {
		try {
			return HashGenerator.encryptSHA256(new String(password));
		}
		catch (final Exception e) {
			logger.error("Error while encrypting password!", e);
		}

		return "";
	}

	/*
	 * (non-Javadoc)
	 * @see jakarta.security.enterprise.identitystore.PasswordHash#verify(char[], java.lang.String)
	 */
	@Override
	public boolean verify(char[] password, String hashedPassword) {
		try {
			return HashGenerator.encryptSHA256(new String(password)).equals(hashedPassword);
		}
		catch (final Exception e) {
			logger.error("Error while encrypting password!", e);
		}

		return false;
	}

}
